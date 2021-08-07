//! Metadata regarding user-facing documentation
use once_cell::sync::Lazy;
use serde::{Serialize, Deserialize};
use std::{
  collections::{BTreeMap},
  fs::{self, File},
  io::{BufReader},
  path::{ PathBuf},
  sync::{RwLock},
};
use strum::{AsRefStr, EnumIter};
use syn::{
  Attribute,
  AttributeArgs,
  Error,
  parse_str,
  Lit,
  LitInt,
  LitStr,
  Meta,
  MetaList,
  MetaNameValue,
  NestedMeta,
  Path,
  Result,
};

/// Line ending 
const END: &str = "  \n";
/// Output directory for persisting data between compilation-time and run-time
pub static OUTPUT_DIR_PATH: &str = "user_doc";
/// Output file name for persisting data between compilation-time and run-time
static OUTPUT_FILE_NAME: &str = "user_doc.json";
/// The repository of all active docs 
pub static DOCS: Lazy<RwLock<DocDict>> = Lazy::new(|| {
  RwLock::new(DocDict(BTreeMap::new()))
});
/// How much text to show in previews
pub const PREVIEW_TEXT_LENGTH: usize = 16;

#[derive(Clone, Serialize, Deserialize, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// A documentation or a Docdict 
/// Each documentable has a name string
pub enum Documentable {
  Doc(String, String),
  BoxedDocDict(String, Box<DocDict>),
}

impl core::fmt::Display for Documentable {
  fn fmt(&self, f: &'_ mut core::fmt::Formatter) -> core::fmt::Result {
    match self {
      Self::Doc(ref name, ref text) => {
        write!(
          f, "Documentable::Doc(`{}`: {})", 
          name, 
          if text.len() > PREVIEW_TEXT_LENGTH { 
            let mut text = text.to_string(); 
            text.truncate(PREVIEW_TEXT_LENGTH-3);
            format!("{}...", text) 
          } else {
            text.to_string()
          }
        )
      },
      Self::BoxedDocDict(ref name, ref boxed_doc_dict) => {
        write!(
          f, "Documentable::BoxedDocDict(`{}`: {} subchapters)", 
          name, 
          boxed_doc_dict.len()
        )
      }
    }
  }
}

#[allow(clippy::enum_variant_names)]
#[derive(AsRefStr, Clone, EnumIter, Debug, Eq, Ord, PartialOrd, PartialEq)]
#[strum(serialize_all = "snake_case")]
/// Attribute helpers
/// 
/// When written in snake case (eg. ChapterNum -> chapter_num),
/// the _variants_ of this enum correspond to the 
///
/// - _helper attributes_ supported by the `user_doc_item` derive macro:
/// ```ignore
/// #[derive(user_doc_item)]
/// // This line will NOT be in the produced documentation.
/// struct SomeStruct {
///   #[chapter_name("A chapter")]
///   #[chapter_num(333)]
///   // This line will be in the produced documentation.
///   some_field: u8
/// }
/// ```  
/// - _arguments_ supported by the `user_doc_fn` attribute macro:
/// ```ignore
/// #[user_doc_fn(chapter_name("A chapter"), chapter_num(333))]
/// // This line will be in the produced documentation.
/// fn some_fn() -> () {}
/// ```  
///
/// The preceding examples both produce the same user-facing documentation.
/// If both examples occurred in the same project, however, one would 
/// overwrite the other. 
pub enum HelperAttr {
  /// Number of this chapter 
  ChapterNum(usize),
  /// Number-path up to and including this chapter 
  ChapterNumSlug(Vec<usize>),
  /// Name of this chapter 
  ChapterName(String),
  /// Name-path up to and including this chapter 
  ChapterNameSlug(Vec<String>),
}

impl From<&HelperAttr> for Path {
  fn from(h: &HelperAttr) -> Self {
    parse_str::<Self>(h.as_ref()).expect("Must create path from HelperAttr")
  }
}
impl HelperAttr {
  /// Instantiate from an AttributeArgs (Vec<NestedMeta>)
  #[allow(clippy::ptr_arg)]
  pub fn from_attribute_args(a: &AttributeArgs) -> Result<Vec<Self>> {
    let mut selves = Vec::with_capacity(a.len());
    for nested_meta in a {
      match nested_meta {
        NestedMeta::Meta(ref meta) => {
          match meta {
            Meta::Path(ref path) => {
              return Err(Error::new_spanned(
                path, "unsupported attribute subpath"
              ))
            },
            Meta::List(MetaList{ref path, ref nested, ..}) => {
              if path.is_ident(Self::ChapterNameSlug(vec![]).as_ref()) {
                let mut slugs = vec![];
                for nested_meta in nested.iter() {
                  match nested_meta {
                    NestedMeta::Lit(Lit::Str(ref lit_str)) => {
                      slugs.push(lit_str.value());
                    },
                    _ => {
                      return Err(Error::new_spanned(
                        nested_meta, "Unsupported nested meta attribute "
                      ))
                    }
                  }
                }
                selves.push(Self::ChapterNameSlug(slugs));
              } else if path.is_ident(Self::ChapterNumSlug(Vec::new()).as_ref()) {
                let mut slugs = vec![];
                for nested_meta in nested.iter() {
                  match nested_meta {
                    NestedMeta::Lit(Lit::Int(ref lit_int)) => {
                      slugs.push(lit_int.base10_parse()?);
                    },
                    _ => {
                      return Err(Error::new_spanned(
                        nested_meta, "Unsupported nested meta attribute "
                      ))
                    }
                  }
                }
                selves.push(Self::ChapterNumSlug(slugs));
              } else {
                return Err(Error::new_spanned(
                  path, "Unsupported meta list path"
                ));
              }
            },
            Meta::NameValue(MetaNameValue{ref path, ref lit, ..}) => {
              if path.is_ident(Self::ChapterName(String::new()).as_ref()) {
                match lit {
                  Lit::Str(ref lit_str) => {
                    selves.push(Self::ChapterName(lit_str.value()));
                  },
                  bad_lit => {
                    return Err(Error::new_spanned(bad_lit, "Unsupported chapter name literal"));
                  }
                }
              } else if path.is_ident(Self::ChapterNum(0usize).as_ref()) {
                match lit {
                  Lit::Int(ref lit_int) => {
                    selves.push(Self::ChapterNum(lit_int.base10_parse()?));
                  },
                  bad_lit => {
                    return Err(Error::new_spanned(bad_lit, "Unsupported chapter number literal"));
                  }
                }
              } else {
                return Err(Error::new_spanned(
                  path, "unrecognized helper attribute inner"
                ));
              }
            }
          }
        },
        _ => {
          return Err(Error::new_spanned(
            nested_meta, "unrecognized helper attribute"
          ));
        }
      }
    }
    Ok(selves)
  }
  
  /// Instantiate from an Attribute if said Attribute represents a valid HelperAttr
  pub fn from_attribute(a: &Attribute) -> Result<Self> {
    let Attribute{path, tokens: _, ..} = a;
    if path.is_ident(&Self::ChapterNum(0).as_ref()) {
      let chapter_num_lit_int = a.parse_args::<LitInt>()?;
      Ok(Self::ChapterNum(chapter_num_lit_int.base10_parse()?))
    } else if path.is_ident(&Self::ChapterName(String::new()).as_ref()) {
      let chapter_name_lit_str = a.parse_args::<LitStr>()?;
      Ok(Self::ChapterName(chapter_name_lit_str.value()))
    } else if path.is_ident(&Self::ChapterNameSlug(Vec::new()).as_ref()) {
      let meta = a.parse_meta()?;
      match meta {
        Meta::List(MetaList{nested, ..}) => {
          let segments: Vec<String> = nested.iter().filter_map(
            |nested_meta| {
              match nested_meta {
                NestedMeta::Lit(Lit::Str(lit_str)) => Some(lit_str.value()),
                _ => None
              }
            }
          ).collect();
          Ok(Self::ChapterNameSlug(segments))
        },
        bad => Err(Error::new_spanned(bad, "unrecognized attribute payload for chapter_name_slug"))
      }
    } else if path.is_ident(&Self::ChapterNumSlug(Vec::new()).as_ref()) {
      let meta = a.parse_meta()?;
      match meta {
        Meta::List(MetaList{nested, ..}) => {
          let mut segments_results: Vec<Result<usize>> = nested.iter().filter_map(
            |nested_meta| {
              match nested_meta {
                NestedMeta::Lit(Lit::Int(lit_int)) => Some(
                  lit_int.base10_parse()
                ),
                _ => None
              }
            }
          ).collect();
          let mut segments: Vec<usize> = Vec::with_capacity(segments_results.len());
          for segment_result in segments_results.drain(0..) {
            segments.push(segment_result?);
          }
          Ok(Self::ChapterNumSlug(segments))
        },
        bad => Err(Error::new_spanned(bad, "unrecognized attribute payload for chapter_name_slug"))
      }
    } else {
      Err(Error::new_spanned(
        path, "unrecognized helper attribute"
      ))
    }
  }
}

/// The level of a markdown header 
pub type HeaderLevel = u8;
/// Constrain the level of a mark header
fn constrain_header_level(header_level: HeaderLevel) -> HeaderLevel{
  header_level.clamp(0, 6)
}
/// Make a markdown header 
fn make_header(header_level: HeaderLevel) -> String {
  let mut s = String::new();
  for _i in 0..header_level {
    s.push('#');
  }
  s
}
/// Make a markdown  named header 
fn make_named_header(header_level: HeaderLevel, header_name: &str) -> String {
  format!("{} {}", make_header(header_level), header_name)
}
/// Make a markdown separator 
fn make_separator() -> String {
  "---".to_string()
}
fn make_chapter_num_string(chap_nums_vec: &[usize]) -> String {
  let mut s = String::new();
  for n in chap_nums_vec.iter() {
    s = format!("{}.{}", s, n);
  }
  s
}
impl core::cmp::PartialEq<&str> for Documentable {
  fn eq(&self, rhs: &&str) -> bool {
    match self {
      Self::Doc(_, contents) => contents.eq(rhs),
      _ => false 
    }
  }
}
impl Documentable {
  /// Get a mutable reference to the inner chapter (BoxedDocDict) contained if any 
  pub fn get_inner_boxed_doc_dict_mut_ref(&mut self) -> Option<&mut Box<DocDict>> {
    match self {
      Self::Doc(_, _) => None,
      Self::BoxedDocDict(_, ref mut boxed_doc_dict) => Some(boxed_doc_dict),
    }
  }
  
  /// Get this item's name 
  pub fn name(&self) -> String {
    match self {
      Self::Doc(ref name, .. ) => { name.clone() },
      Self::BoxedDocDict(ref name, .. ) => { name.clone() },
    }
  }
  
  /// Coerce this documentation to a string of markdown 
  pub fn to_string(
    &self, 
    header_level: HeaderLevel,
    chapter_nums_opt: Option<Vec<usize>>,
  ) -> String {
    let chapter_nums = chapter_nums_opt.unwrap_or_default();
    let header_level = constrain_header_level(header_level);
    let mut s = String::new();
    match self {
      Documentable::Doc(ref name, ref doc_text) => {
        s.push_str(&format!(
          "{}{}{}", 
          make_named_header(header_level, name),
          END,
          doc_text
        ));
      },
      Documentable::BoxedDocDict(ref name, ref boxed_doc_dict) => {
        let count = boxed_doc_dict.len();
        for (subchapter_num, (subchapter_name, documentable)) in boxed_doc_dict.iter() {
          let mut chapter_nums = chapter_nums.clone();
          chapter_nums.push(*subchapter_num);
          let sep_string = if *subchapter_num < count - 1 { make_separator() } else { String::new() };
          s.push_str(&format!(
            "{}{}
            {}{}
            {}{}
            {}{}
            {}{}",
            make_named_header(header_level, name), END,
            make_chapter_num_string(&chapter_nums), END,
            make_named_header(header_level+1, subchapter_name), END,
            documentable.to_string(
              header_level + 1,
              Some(chapter_nums),
            ), END,
            sep_string, END
          ))
        }
      }
    }
    
    s
  }
}

/// Entries in the DocDict take this form 
pub type DocDictEntryValueType = (String, Documentable);
/// A tree-dictionary of docs 
pub type DocDictTree = BTreeMap<usize, DocDictEntryValueType>;
#[derive(Clone, Serialize, Deserialize, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// A dictionary of doc
pub struct DocDict(pub DocDictTree);
impl DocDict {
  /// Add an entry to a doc dict
  pub fn add_entry(
    &mut self,
    documentable: Documentable, 
    name_opt: Option<String>,
    number_opt: Option<usize>,
    overwrite_opt: Option<bool>,
  ) -> anyhow::Result<()> {
    let number = number_opt.unwrap_or_default(); 
    let name = name_opt.unwrap_or_else(|| documentable.name()); // take name from documentable
    let (
      already_has_number,
      already_has_name,
    ) = self.iter().fold(
      (false, false),
      |(has_number, has_name), (num_i, (nam_i, _))| (
        has_number || num_i == &number,
        has_name || nam_i == &name,
      )
    );
    // std::println!("already_has_number {}: {}, already_has_name `{}` {}", number, already_has_number, name, already_has_name);
    match (already_has_number, already_has_name) {
      (false, false) => {
        self.insert(number, (name, documentable));
        Ok(())
      },
      (false, true) => {
        // Duplicate names are acceptable
        self.insert(number, (name, documentable));
        Ok(())
      },
      (true, false) => {
        // Duplicate numbers are not acceptable
        let number = self.find_next_entry_number();
        self.insert(
          number, (name, documentable)
        );
        Ok(())
      },
      (true, true) => {
        let overwrite_falsy_message = format!(
          "Attempted to insert duplicate entry for (chapter, title) ({} {}). 
          \nTry setting overwrite_opt param to Some(true)",
          number, name
        );
        if let Some(must_overwrite) = overwrite_opt {
          if must_overwrite {
            self.insert(number, (name, documentable));
            Ok(())
          } else {
            anyhow::bail!(overwrite_falsy_message)
          }
        } else {
          // Duplicate numbers are not acceptable
          anyhow::bail!(overwrite_falsy_message);
        }
      },
    }
  }
  
  /// Add the path specified to a doc dict, filling with empty subtrees to get there.
  /// Return Ok(()) on success.
  /// - If the path exists, fail unless `overwrite_opt` contains `true` 
  pub fn add_path(
    &mut self, 
    name_opt: &Option<String>,
    documentable_opt: Option<Documentable>,
    overwrite_opt: Option<bool>, 
    path_names: &[String],
    path_numbers: &[usize],
  ) -> anyhow::Result<()> {
    use anyhow::Context;
    // let _name = name_opt.clone().unwrap_or_default();
    let mut i = 0;
    let mut subdict = self;
    let mut paths_found = vec![false; path_numbers.len()];
    while i < path_numbers.len() {
      let chapter_num = path_numbers.get(i).cloned().unwrap_or_else(|| {
        subdict.find_next_entry_number()
      });
      let chapter_name = path_names.get(i).or_else(|| name_opt.as_ref()).cloned().unwrap_or_default();
      let empty_chapter = Documentable::BoxedDocDict(chapter_name.clone(), Box::new(DocDict(BTreeMap::new())));
      if !subdict.iter().any(|(num, (_name, _contents))| num == &chapter_num ) {
        let documentable = if i == path_numbers.len() - 1 {
          // this is the target point of the path, where the optional documentable should go
          documentable_opt.clone().unwrap_or(empty_chapter)
        } else {
          // the default empty chapter 
          empty_chapter 
        };
        // add the new chapter 
        subdict.add_entry(
          documentable, 
          Some(chapter_name.clone()),
          Some(chapter_num), 
          overwrite_opt
        )?;
        // std::println!("updated subdict {:#?}", subdict );
      } else {
        // just mark that this chapter already existed
        paths_found[i] = true;
      }
      if i < path_numbers.len() - 1 {
        // Get the inner sub dictionary 
        subdict = subdict.get_mut(&chapter_num)
          .with_context(|| format!(
            "Must get new chapter ({} (name: {}))",
            chapter_num, 
            chapter_name, 
          ))?
          .1.get_inner_boxed_doc_dict_mut_ref()
          .with_context(|| format!(
            "Must get newly inserted chapter ({} (name: {}))",
            chapter_num, 
            chapter_name, 
          ))?;
      }
      // Increment depth
      i+=1;
    }
    Ok(())
  }
  
  /// Find the next available (unused) entry number in a doc dict 
  pub fn find_next_entry_number(&self) -> usize {
    let mut n = 0usize;
    
    while self.keys().cloned().any(|x| x == n) {
      n+=1;
    }
    n
  }

  #[allow(clippy::unnecessary_unwrap)]
  /// Get an immutable reference to an entry at the specified numeric path. Returns `None` if the
  /// a path is not present.
  pub fn get_entry_at_numeric_path(&self, path: &[usize]) -> Option<&DocDictEntryValueType> {
    let mut map_pointer: &Self = self;
    let max_depth = path.len() - 1;
    for (depth, index) in path.iter().enumerate() {
      let entry_opt = map_pointer.get(index);
      if entry_opt.is_some()  {
        if depth == max_depth {
          // this must be the record 
          return entry_opt
        } else {
          match &entry_opt.unwrap().1 {
            Documentable::Doc(_, _) => {
              // no further traversal is possible 
              return None;
            },
            Documentable::BoxedDocDict(_name, ref boxed_sub_dict) => {
              // point to the next depth for further traversal
              map_pointer = &*boxed_sub_dict;
            },
          }
        }
      } else {
        return None
      }
    }
    None 
  }

  #[allow(clippy::unnecessary_unwrap)]
  /// Get a mutable reference to an entry at the specified numeric path. Returns `None` if the
  /// a path is not present.
  pub fn get_mut_entry_at_numeric_path(&mut self, path: &[usize]) -> Option<&mut DocDictEntryValueType> {
    let mut map_pointer: &mut Self = self;
    let max_depth = path.len() - 1;
    for (depth, index) in path.iter().enumerate() {
      let entry_opt = map_pointer.get_mut(index);
      if entry_opt.is_some()  {
        if depth == max_depth {
          // this must be the record 
          return entry_opt
        } else {
          match &mut entry_opt.unwrap().1 {
            Documentable::Doc(_, _) => {
              // no further traversal is possible 
              return None;
            },
            Documentable::BoxedDocDict(_name, ref mut  boxed_sub_dict) => {
              // point to the next depth for further traversal
              map_pointer = &mut *boxed_sub_dict;
            },
          }
        }
      } else {
        return None
      }
    }
    None 
  }
}
impl core::ops::Deref for DocDict {
  type Target = DocDictTree;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
impl core::ops::DerefMut for DocDict {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}


/// Make a string from a DocDict key 
pub fn make_string_key(num: usize, name: String) -> String {
  format!("({})({})", num, name)
}

/// Save the global store to a file path at the given directory path
/// This is necessary because the global store cannot persist between compilation time and runtime
/// See [OUTPUT_DIR_PATH]
pub fn save_global_docs_to_path(
  dir_path_str_opt: Option<String>,
  file_name_str_opt: Option<String>,
  // overwrite_opt: Option<bool>,
) -> anyhow::Result<()> {
  use anyhow::Context;
  let dir_path: PathBuf = if let Some(dir_path_str) = dir_path_str_opt {
    dir_path_str.into()
  } else {
    std::env::temp_dir().join(OUTPUT_DIR_PATH)
  };
  // let dir_path_str: &str = &dir_path_str_opt.unwrap_or(OUTPUT_DIR_PATH.to_string());
  // let dir_path = Path::new(dir_path_str);
  let file_name: &str = &file_name_str_opt.unwrap_or_else(|| OUTPUT_FILE_NAME.to_string());
  // let overwrite = overwrite_opt.unwrap_or_default();
  // Create the path if it doesn't exist
  if !dir_path.is_dir() {
    fs::create_dir_all(dir_path.clone())?;
  } 
  let complete_path = dir_path.join(file_name);
  // std::println!("saving to {:?}", complete_path);
  let file = File::create(complete_path)?;
  let docs = &*DOCS;
  let docs_read_lock = docs.read()
    .map_err(|poison_error| {
      anyhow::anyhow!(format!("{:#?}", poison_error))
    })
    .with_context(|| "Must get read lock on DOCS")?;
  serde_json::to_writer(file, &*docs_read_lock)
    .with_context(|| {
      format!(
        "Must write JSON from DOCS to file: \nResult:\n{:#?}",
        serde_json::to_string(&*docs_read_lock)
      )
    })?;
  Ok(())
}

/// Load the docs from a file path 
/// This is necessary because the global store cannot persist between compilation time 
/// and runtime.
pub fn load_global_docs_to_path (
  dir_path_str_opt: Option<String>,
  file_name_str_opt: Option<String>,
) -> anyhow::Result<()> {
  use anyhow::Context;
  // let dir_path_str: &str = &dir_path_str_opt.unwrap_or("./".to_string());
  let dir_path: PathBuf = if let Some(dir_path_str) = dir_path_str_opt {
    dir_path_str.into()
  } else {
    std::env::temp_dir().join(OUTPUT_DIR_PATH)
  };
  let file_name: &str = &file_name_str_opt.unwrap_or_else(|| OUTPUT_FILE_NAME.to_string());
  // let overwrite = overwrite_opt.unwrap_or_default();
  // Create the path if it doesn't exist
  if !dir_path.is_dir() {
    anyhow::bail!(
      format!(
        "The target docs directory ({:?}) is not a directory",
        dir_path
      )
    );
  } 
  let complete_path: PathBuf = dir_path.join(file_name);
  std::println!("complete_path {:?}", complete_path );
  let file = File::open(complete_path)?;
  let file_reader = BufReader::new(file); 
  let docs = &*DOCS;
  let doc_dict: DocDict = serde_json::from_reader(file_reader)
    .with_context(|| {
      "Must read JSON from file into docs".to_string()
    })?;
  let mut docs_write_lock = docs.write()
    .map_err(|poison_error| {
      anyhow::anyhow!(format!("{:#?}", poison_error))
    })
    .with_context(|| "Must get write lock on DOCS")?;
  std::println!("docs_write_lock {:#?}", docs_write_lock );
  *docs_write_lock = doc_dict;
  Ok(())
}
