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
/// Custom output file path for persisting data 
pub static CUSTOM_OUTPUT_FILE_NAME: Lazy<RwLock<Option<String>>> = Lazy::new(|| 
  RwLock::new(None)
);
/// The repository of all active docs 
pub static DOCS: Lazy<RwLock<DocDict>> = Lazy::new(|| {
  RwLock::new(DocDict(BTreeMap::new(), Vec::new()))
});
/// How much text to show in previews
pub const PREVIEW_TEXT_LENGTH: usize = 16;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// When expanding to directories, this determines the naming scheme 
pub enum DirectoryNamingScheme {
  /// Use the deepest-depth chapter name 
  ChapterName,
  /// Use the deepest-depth chapter number 
  ChapterNumber,
}

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
  /// A blurb to add to the page for the chapter 
  ChapterBlurb(String)
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
              } else if path.is_ident(Self::ChapterBlurb(String::new()).as_ref()) {
                match lit {
                  Lit::Str(ref lit_str) => {
                    selves.push(Self::ChapterBlurb(lit_str.value()));
                  },
                  bad_lit => {
                    return Err(Error::new_spanned(bad_lit, "Unsupported chapter blurb literal"));
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
    } else if path.is_ident(&Self::ChapterBlurb(String::new()).as_ref()) {
      let chapter_name_blurb_str = a.parse_args::<LitStr>()?;
      Ok(Self::ChapterBlurb(chapter_name_blurb_str.value()))
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
#[derive(Clone, Default, Serialize, Deserialize, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// A (possibly-nested) dictionary of docs
pub struct DocDict(
  /// The dictionary tree 
  pub DocDictTree, 
  /// Backlinks and blurbs
  pub Vec<(Vec<usize>, String)>
);
impl DocDict {
  /// Add an entry to a documentation dictionary
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
  
  /// Add the path specified to a documentation dictionary, filling with empty subtrees to get there.
  /// Return Ok(()) on success.
  /// - If the path exists, fail unless `overwrite_opt` contains `true` 
  pub fn add_path(
    &mut self, 
    chapter_blurb_opt: &Option<String>,
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
      let empty_chapter = Documentable::BoxedDocDict(
        chapter_name.clone(), 
        Box::new(
          DocDict(
            BTreeMap::new(), 
            Vec::new(),
          )
        )
      );
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
        // add the backlink and add - chapter blurb to the parent chapter text 
        if let Some(ref chapter_blurb) = chapter_blurb_opt {
          let back_link = path_numbers.to_vec();
          subdict.1.push((
            back_link,
            chapter_blurb.to_string()
          ));
        }
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
  
  /// Produce a depth-first, _immutable_ iterator over the entries.
  ///
  /// - The iterator item includes a slug of chapters, the current entry node, 
  /// and the number of sub entries for the current  node
  /// The iterator will produce entries for the chapters AS WELL AS entries for the subchapters of
  /// those chapters. 
  pub fn deep_iter(
    &self, 
    start_slug_opt: Option<Vec<usize>>,
  ) -> std::collections::vec_deque::IntoIter<
    (Vec<usize>, &DocDictEntryValueType, usize)
  > {
    use std::collections::VecDeque;
    let mut vv: VecDeque<(Vec<usize>, &DocDictEntryValueType, usize)> = VecDeque::new();
    let start_slug = start_slug_opt.unwrap_or_default();
    if !self.0.is_empty() {
      // go through each root item and check what it contains
      for (k, entry) in self.0.iter() { 
        // record a new slug for this position
        let mut iter_slug = start_slug.clone();
        iter_slug.push(*k);
        if let Documentable::BoxedDocDict(_, dd) = &entry.1 {
          vv.push_back((iter_slug.clone(), entry, (*dd).len())); 
          // add the descent items for this node 
          vv.extend(dd.deep_iter(Some(iter_slug)))
        } else {
          vv.push_back((iter_slug, entry, 0usize)); 
        }
      }
    }
    vv.into_iter()
  }
  
  /// Expand this doc dict into directories at the given _directory_ path
  pub fn expand_into_mdbook_dirs_at_path(
    &self,
    naming_scheme: DirectoryNamingScheme, 
    root_path: &str,
  ) -> anyhow::Result<()> {
    use anyhow::Context;
    const README_NAME:&str = "README";
    let dir_path: PathBuf = root_path.into();
    if !dir_path.is_dir() {
      fs::create_dir_all(dir_path.clone())
        .with_context(|| format!("Must create root path {:?}", dir_path))?;
    }
    let one_indentation = "  ";
    let indent_for_depth = |depth:usize| -> String {
      let v = vec![one_indentation; depth];
      v.join("")
    };
    let mut slugs_to_paths: BTreeMap<Vec<usize>, PathBuf> = BTreeMap::new();
    let mut summary_md_contents = String::from("# Summary");
    if !self.1.is_empty() {
      for chapter_summary_line in self.1.iter() {
        summary_md_contents.push_str(&format!("\n{}  ", chapter_summary_line.1));
      }
    }
    for (iter_slug, (name, documentable), _sub_entries_len) in self.deep_iter(None) {
      let depth = iter_slug.len() - 1;
      if depth == 0 {
        // Prefix Chapters 
        // See: https://rust-lang.github.io/mdBook/format/summary.html#structure
        
      }
      let mut subdir_path = slugs_to_paths.get(&iter_slug[0..iter_slug.len()-1]).cloned()
        .unwrap_or_else(|| dir_path.clone());
      let number = iter_slug.last().expect("must get default name");
      let name = if name.is_empty() {
        format!("{}", number)
      } else {
        format!("{} - {}", number, name)
      };
      let mut contents_name = iter_slug[1..].iter().fold(
        iter_slug[0].to_string(),
        |s, ii| format!("{}.{}",s, ii) 
      );
      match documentable {
        Documentable::Doc(ref doc_name, ref contents) => {
          subdir_path.push(name.clone());
          subdir_path.set_extension("md");
          let out_contents = format!("# {}  \n{}", name, contents);
          fs::write(subdir_path.clone(), out_contents)
            .with_context(|| format!("must write to file {:?}", subdir_path))?;          
          if !doc_name.is_empty() {
            contents_name.push_str(&format!(" - {}", doc_name));
          }
          summary_md_contents.push_str(
            &format!(
              "\n{}- [{}](<{}>)  ",
              indent_for_depth(depth),
              contents_name,
              subdir_path.strip_prefix(dir_path.clone())
                .with_context(|| "Must create subdir path for summary.md".to_string())?
                .to_string_lossy()
            )
          );
        },
        Documentable::BoxedDocDict(chapter_name, boxed_doc_dict) => {
          // update path 
          match &naming_scheme {
            // Use the deepest-depth chapter name 
            DirectoryNamingScheme::ChapterName => {
              subdir_path.push(name.clone());
            },
            // Use the deepest-depth chapter number 
            DirectoryNamingScheme::ChapterNumber => {
              subdir_path.push(
                iter_slug
                  .iter()
                  .last()
                  .expect("must get last slug element")
                  .to_string()
              );
            },
          }
          // create folder 
          fs::create_dir_all(subdir_path.clone())
            .with_context(|| format!("Must create subdir path {:?}", subdir_path))?;
          slugs_to_paths.insert(iter_slug.clone(), subdir_path.to_path_buf());
          // add chapter level-readme entry to summary 
          let mut chapter_readme_path = subdir_path.clone();
          chapter_readme_path.push(&README_NAME);
          chapter_readme_path.set_extension("md");
          contents_name.push_str(&format!(" - {}", chapter_name));
          summary_md_contents.push_str(
            &format!(
              "\n{}- [{}](<{}>)  ",
              indent_for_depth(depth),
              contents_name,
              chapter_readme_path.strip_prefix(dir_path.clone())
                .with_context(|| format!("Must create relative chapter readme path from {:?}", dir_path))?
                .to_string_lossy()
            )
          );
          // create chapter-level readme
          let mut chapter_readme_contents = format!("# {}", chapter_name);
          
          if !boxed_doc_dict.1.is_empty() {
            // Chapter blurbs 
            for (chapter_i, chapter_documentable) in boxed_doc_dict.0.iter() {
              let mut forward_link: PathBuf = match &naming_scheme {
                // Use the deepest-depth chapter name 
                DirectoryNamingScheme::ChapterName => {
                  let chapter_name = chapter_documentable.1.name();
                  if chapter_name.is_empty() {
                    format!("{}", chapter_i)
                  } else {
                    format!("{} - {}", chapter_i, chapter_name)
                  }
                },
                // Use the deepest-depth chapter number 
                DirectoryNamingScheme::ChapterNumber => {
                  chapter_i.to_string()
                },
              }.into();
              forward_link.set_extension("md");
              let chapter_readme_blurb = boxed_doc_dict.1.iter()
                .find(|(backlink, _blurb)| backlink == &iter_slug)
                .map(|(_backlink, blurb)| blurb.to_string())
                .unwrap_or_else(|| format!(
                  "\n- [Skip to {} ({})](<{}>)  ", 
                  chapter_documentable.0, 
                  chapter_i,
                  forward_link.to_string_lossy(),
                )); 
              
              chapter_readme_contents.push_str(&format!(
                "\n{}", 
                chapter_readme_blurb
              ));
            }
          }
          fs::write(chapter_readme_path.clone(), chapter_readme_contents)
            .with_context(|| format!("must write to summary.md file {:?}", chapter_readme_path))?;
        },
      }
    }
    
    // create summary md 
    let mut summary_md_path = dir_path;
    summary_md_path.push("SUMMARY");
    summary_md_path.set_extension("md");
    fs::write(summary_md_path.clone(), summary_md_contents)
      .with_context(|| format!("must write to summary.md file {:?}", summary_md_path))?;
    Ok(())
  }
  
  /// Find the next available (unused) entry number in a documentation dictionary 
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


/// Get the persistence directory path
pub fn get_persistence_dir_path () -> PathBuf {
  std::env::temp_dir().join(OUTPUT_DIR_PATH)
}

/// Get the full persistence directory+file path
pub fn get_persistence_file_path () -> anyhow::Result<PathBuf> {
  CUSTOM_OUTPUT_FILE_NAME
    .read()
    .map_or_else(
      |_| anyhow::bail!("Must get read lock on CUSTOM_OUTPUT_FILE_NAME"),
      |custom_output_file_name_lock| {
        Ok(get_persistence_dir_path().join(
          custom_output_file_name_lock
            .as_ref()
            .cloned()
            .unwrap_or_else(|| OUTPUT_FILE_NAME.to_string())
        ))    
      }
    )
}

/// Make a string from a DocDict key 
pub fn make_string_key(num: usize, name: String) -> String {
  format!("({})({})", num, name)
}

/// Save the global store to a file
///
/// - This is necessary because the global store cannot persist between compilation time and runtime
/// - If no directory is specified it will create a directory that includes the name of the 
/// crate from which the proc macro is called 
/// See [OUTPUT_DIR_PATH]
pub fn persist_docs() -> anyhow::Result<()> {
  use anyhow::Context;
  let dir_path = get_persistence_dir_path();
  // Create the path if it doesn't exist
  if !dir_path.is_dir() {
    fs::create_dir_all(dir_path.clone())?;
  } 
  let complete_path: PathBuf = get_persistence_file_path()?;
  std::println!("saving to {:?}", complete_path);
  let file = File::create(complete_path)?;
  let docs = &*DOCS;
  let docs_read_lock = docs.read()
    .map_err(|poison_error| {
      anyhow::anyhow!(format!("{:#?}", poison_error))
    })
    .with_context(|| "Must get read lock on DOCS")?;
  std::println!("{:#?}", docs_read_lock );
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
///
/// - If given a DocDict, it will load into that DocDict. Otherwise, it will load
/// into the global doc dict, overwriting  
/// This is necessary because the global store cannot persist between compilation time 
/// and runtime.
pub fn load_global_docs (
  file_name_opt: Option<&str>,
  doc_dict_opt: Option<&mut DocDict>,
) -> anyhow::Result<()> {
  use anyhow::Context;
  let complete_path: PathBuf = 
    if let Some(file_name) = file_name_opt {
      get_persistence_dir_path()
        .join(file_name)
    } else {
      get_persistence_file_path()?
    };
    // Fail if the path doesn't exist
  if !complete_path.is_file() {
    anyhow::bail!(
      format!(
        "The target docs directory ({:?}) is not a file",
        complete_path
      )
    );
  } 
  std::println!("loading global docs from complete_path {:?}", complete_path );
  let file = File::open(complete_path)?;
  let file_reader = BufReader::new(file); 
  let docs = &*DOCS;
  let doc_dict: DocDict = serde_json::from_reader(file_reader)
    .with_context(|| {
      "Must read JSON from file into docs".to_string()
    })?;
  match doc_dict_opt {
    Some(existing_doc_dict) => {
      *existing_doc_dict = doc_dict;
    }
    None => {
      let mut docs_write_lock = docs.write()
      .map_err(|poison_error| {
        anyhow::anyhow!(format!("{:#?}", poison_error))
      })
      .with_context(|| "Must get write lock on DOCS")?;
      // std::println!("docs_write_lock {:#?}", docs_write_lock );
      *docs_write_lock = doc_dict;
    }
  }
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn deep_iter() {
    let mut d:DocDict = DocDict(
      BTreeMap::new(), 
      vec![
        (vec![], String::from("A book about some things. Words sdrwo."), ),
        (vec![], String::from("\nAnd some other things.")),
      ]
    );
    
    let slugs = vec![
      vec![ 
        (1, "buff".to_string()), 
        (2, "aztec".to_string()), 
        (3, "priestess".to_string()),
      ],
      vec![ 
        (1, "buff".to_string()), 
        (2, "aztec".to_string()), 
        (4, "priest".to_string()),
      ],
      vec![ 
        (1, "buff".to_string()), 
        (2, "aztec".to_string()), 
        (5, "eagle warrior".to_string()),
      ],
      vec![ 
        (1, "buff".to_string()), 
        (3, "maya".to_string()), 
        (2, "princess".to_string()),
      ],
      vec![ 
        (1, "buff".to_string()), 
        (3, "maya".to_string()), 
        (5, "prince".to_string()),
      ],      
    ];
    let target_ord = vec![
      slugs[0][0].clone(), 
      slugs[0][1].clone(), 
      slugs[0][2].clone(), 
      slugs[1][2].clone(), 
      slugs[2][2].clone(),
      slugs[3][1].clone(), 
      slugs[3][2].clone(), 
      slugs[4][2].clone(),
    ];
    let target_num_slugs =  vec![
      vec![1],
      vec![1, 2],
      vec![1, 2, 3],
      vec![1, 2, 4],
      vec![1, 2, 5],
      vec![1, 3],
      vec![1, 3, 2],
      vec![1, 3, 5],
    ];
    let target_sub_entries_lens =  vec![
      2,
      3,
      0,
      0,
      0,
      2,
      0,
      0,
    ];
    for slug in slugs {
      let (path_numbers, path_names): (Vec<_>, Vec<_>) = slug.iter().cloned().unzip();
      let name = slug.last().unwrap().1.to_string();
      d.add_path(
        &None,
        &Some(name.clone()),
        Some(Documentable::Doc(name.clone(), "dummy".to_string())),
        None, 
        &path_names,
        &path_numbers
      ).expect("must add path");
    }
    for (i, (iter_slug, (name, documentable), sub_entries_len)) in d.deep_iter(None).enumerate() {
      let target = &target_ord[i];
      let target_num_slug = &target_num_slugs[i];
      let target_sub_entries_len = target_sub_entries_lens[i];
      assert_eq!(
        &target.1,
        name,      
        "{} th target name must match", i  
      );
      assert_eq!(
        target_num_slug,
        &iter_slug,      
        "{} th target slug must match", i  
      );
      assert_eq!(
        target_sub_entries_len,
        sub_entries_len,
        "{} th target sub entries length must match", i  
      );
      
      std::println!("{} {}", i, documentable);
    }
  }
}
