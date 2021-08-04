#![allow(unused_imports)]

use user_doc::*;

#[test]
fn must_extract_doc_comments () {
  #[user_doc]
  /// These are doc comments
  pub fn dummy() {}
}
