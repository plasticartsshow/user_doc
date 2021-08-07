use once_cell::sync::Lazy;
use serde::{Serialize, Deserialize};

use std::{
  collections::{BTreeMap},
  fs::{self, File},
  io::{BufReader},
  path::{ PathBuf},
  sync::{RwLock},
};
/// Line ending 
const END: &str = "  \n";
/// Output directory for persisting data between compilation-time and run-time
static OUTPUT_DIR_PATH: &str = "user_doc";
/// Output file name for persisting data between compilation-time and run-time
static OUTPUT_FILE_NAME: &str = "user_doc.json";
/// The repository of all active docs 
pub static DOCS: Lazy<RwLock<DocDict>> = Lazy::new(|| {
  RwLock::new(DocDict(BTreeMap::new()))
});

#[derive(Clone, Serialize, Deserialize, Debug, Eq, Ord, PartialEq, PartialOrd)]
/// A documentation or a Docdict 
/// Each documentable has a name string
pub enum Documentable {
  Doc(String, String),
  BoxedDocDict(String, Box<DocDict>),
}

/// How much text to show in previews
pub const PREVIEW_TEXT_LENGTH: usize = 16;
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

/// The level of a header 
pub type HeaderLevel = u8;
fn constrain_header_level(header_level: HeaderLevel) -> HeaderLevel{
  header_level.clamp(0, 6)
}
fn make_header(header_level: HeaderLevel) -> String {
  let mut s = String::new();
  for _i in 0..header_level {
    s.push('#');
  }
  s
}
fn make_named_header(header_level: HeaderLevel, header_name: &str) -> String {
  format!("{} {}", make_header(header_level), header_name)
}
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
/// See [DEFAULT_DOCS_FILE_NAME]
pub fn save_docs_to_path(
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
pub fn load_docs_from_path (
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
