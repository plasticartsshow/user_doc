use once_cell::sync::Lazy;
use std::{
  collections::{BTreeMap},
  sync::{RwLock},
};
/// Line ending 
const END: &str = "  \n";

/// A documentation or a Docdict 
/// Each documentable has a name string
pub enum Documentable {
  Doc(String, String),
  BoxedDocDict(String, Box<DocDict>)
}
/// The level of a header 
pub type HeaderLevel = u8;
fn constrain_header_level(header_level: HeaderLevel) -> HeaderLevel{
  header_level.clamp(0, 6)
}
fn make_header(header_level: HeaderLevel) -> String {
  let mut s = String::new();
  for _i in 0..header_level {
    s.push_str("#");
  }
  s
}
fn make_named_header(header_level: HeaderLevel, header_name: &str) -> String {
  format!("{} {}", make_header(header_level), header_name)
}
fn make_separator() -> String {
  format!("---")
}
fn make_chapter_num_string(chap_nums_vec: &Vec<usize>) -> String {
  let mut s = String::new();
  for n in chap_nums_vec.iter() {
    s = format!("{}.{}", s, n);
  }
  s
}
impl Documentable  {
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
        for ((subchapter_num, subchapter_name), documentable) in boxed_doc_dict.iter() {
          let mut chapter_nums = chapter_nums.clone();
          chapter_nums.push(*subchapter_num);
          let sep_string = if *subchapter_num < count - 1 { make_separator() } else { String::new() };
          s.push_str(&format!(
            "{}{}
            {}{}
            {}{}
            {}{}",
            make_chapter_num_string(&chapter_nums), END,
            make_named_header(header_level, subchapter_name), END,
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

/// A dictionary of docs
pub type DocDict = BTreeMap<(usize, String), Documentable>;
/// Add an entry to a doc dict
pub fn add_entry_to_doc_dict (
  doc_dict: &mut DocDict,
  documentable: Documentable, 
  name_opt: Option<String>,
  number_opt: Option<usize>,
) -> anyhow::Result<()> {
  let number = number_opt.unwrap_or_default(); 
  let name = name_opt.unwrap_or_else(|| documentable.name()); // take name from documentable
  let (
    already_has_number,
    already_has_name,
  ) = doc_dict.keys().fold(
    (false, false),
    |(has_number, has_name), (num, nam)| (
      has_number || num == &number,
      has_name || name == name,
    )
  );
  match (already_has_number, already_has_name) {
    (false, false) => {
      doc_dict.insert((number, name), documentable);
      Ok(())
    },
    (false, true) => {
      // Duplicate names are acceptable
      doc_dict.insert((number, name), documentable);
      Ok(())
    },
    (true, false) => {
      // Duplicate numbers are not acceptable
      doc_dict.insert((find_next_entry_number_in_doc_dict(&doc_dict), name), documentable);
      Ok(())
    },
    (true, true) => {
      // Duplicate numbers are not acceptable
      anyhow::bail!(
        format!(
          "Attempted to insert duplicate entry for (chapter, title) ({} {})",
          number, name
        )
      )
    },
  }
}

/// Find the next available (unused) entry number in a doc dict 
pub fn find_next_entry_number_in_doc_dict(d: &DocDict) -> usize {
  let mut n = 0usize;
  let nums = d.keys().map(|(n, _)| n).cloned().collect::<Vec<_>>();
  while nums.contains(&n) {
    n+=1;
  }
  n
}

/// The repository of all active docs 
pub static DOCS: Lazy<RwLock<DocDict>> = Lazy::new(|| RwLock::new(BTreeMap::new()));

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
