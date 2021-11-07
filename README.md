[![Workflow Status](https://github.com/plasticartsshow/user_doc/workflows/Rust/badge.svg)](https://github.com/plasticartsshow/user_doc/actions?query=workflow%3A%22Rust%22)
![Maintenance](https://img.shields.io/badge/maintenance-experimental-blue.svg)

# user_doc

Don't define documentation in multiple places.

The attribute [user_doc_fn](macro@doc_proc_macro::user_doc_fn) and derive
[user_doc_item](macro@doc_proc_macro::user_doc_item) macros
capture documentation from comments and make the contents
available at runtime.

Each macro invocation fills in nodes of a [DocDict](doc_data::DocDict) containing all the documentation
captured during a build. At runtime, a global copy of the tree generated at compile time is
available at [DOCS](doc_data::DOCS) via a RwLock.

---

## The macro options
These macros are easily configured with the following helper attributes(for user_doc_item) / arguments (for user_doc_fn):

- [chapter_blurb](doc_data::HelperAttr::ChapterBlurb): A string literal that will be added to
the containing chapter to describe this documentation.
- [chapter_name](doc_data::HelperAttr::ChapterName): A string literal that names this chapter.
- [chapter_name_slug](doc_data::HelperAttr::ChapterNameSlug): A comma-separated list of string literals
that name a path of chapters.
- [chapter_num](doc_data::HelperAttr::ChapterNum): An integer literal corresponding to the number of this chapter.
- [chapter_num_slug](doc_data::HelperAttr::ChapterNumSlug): A comma-separated list of integer literals
corresponding to a path of chapters.

## How to use `user_doc_fn` on a _doc commented_ function definition:
Imagine that the documentation for the function `call_this_function` should
be visible to the user at runtime.
```rust
#[user_doc_fn(
  chapter_num_slug(1, 3, 5),
  chapter_name_slug(
    "A Slaying in Luton",
    "The Trouble About Ipswich",
    "All Along the Weary M-5 Motorway",
  ),
)]


/// The parenchyma isn't as stiff as usual. It looks almost floppy.
/// I stick out a hand to touch it. It sucks my fingertips forward.
/// When I pull my hand back, a hanging bridge of sap follows.
pub fn call_this_function() -> bool { true }
```

The commented lines (from "The parenchyma" to "sap follows.") will be
captured and assigned a location in a tree hierarchy with numbered/named nodes:
- 1. "A Slaying in Luton"
- 3. "The Trouble About Ipswich"
- 5. "All Along the Weary M-5 Motorway"

So that at runtime:
```rust
doc_data::load_global_docs_to_path(
  None, None
).expect("must load docs from path");
let docs = &*doc_data::DOCS;
let docs_read_lock = docs.read().expect("must get read guard on global docs");


 assert_eq!(
  docs_read_lock.get_entry_at_numeric_path(
    &[1,3,5] // corresponds to the `chapter_num_slug` argument in the macro call
  ).expect("must find test entry").1,
  " The parenchyma isn\\'t as stiff as usual. It looks almost floppy.\
    \n I stick out a hand to touch it. It sucks my fingertips forward.\
    \n When I pull my hand back, a hanging bridge of sap follows.",
);
```

The doc comment has been inserted in a tree structure. wow. Now, it can be shown to the user
at runtime.

## How to use `user_doc_item` on a _doc-commented_ struct or enum definition.
Imagine the same usage case, but this time, the documentation is attached to struct or enum definition.
When working with structs or enums, the outer attribute comments are NOT captured.

```rust
#[derive(user_doc_item, Clone, Debug, PartialEq, Eq)]
/// This comment WILL NOT BE captured for user docs.
pub enum Idiot {
  #[chapter_num(23)] // should be overriden by slug
  #[chapter_num_slug(1, 3, 4)]
  #[chapter_name("The House of Almond Blossoms")]
  /// He took a look at the card I showed him. His brows scrunched up like he smelled something offensive.
  /// Could he tell it was fake? Everyone sweats in heat like this. If they don't, it means they're about to keel over from dehydration anyway. Still, I felt like I was sweating more than usual.
  /// We stood frozen for a moment. It seemed an eon. Then he mercifully broke the silence.
  /// "I can't read those. I'm just supposed to stand here and not let anyone pass."
  /// I considered. I didn't want to get him in trouble – he was clearly new. Still, it was me or him.
  /// "Oh," I said as casually as I could, "It says you're to let me through. But don't let anyone else through after me. It says that too.""
  Kid(u16),
}


let docs = &*user_doc::DOCS;
let docs_read_lock = docs.read().expect("must get read guard on global docs");
std::println!("docs_read_lock {:?} {:#?}", std::time::Instant::now(), *docs_read_lock );
assert_eq!(
  docs_read_lock.get_entry_at_numeric_path(&
    [1,3,4], // corresponds to the `chapter_num_slug` helper attribute in the macro call
  ).expect("must find test entry").0,
  String::from("The House of Almond Blossoms"),
);
```
Here, the `chapter_num_slug` helper attribute has overriden the `chapter_num` attribute.
Slug-style (number or name) attributes and arguments will always take precedence.

## How to prepare a DocDict for use with `mdbook`

To expand the global doc store into a hierarchy at the path "tests/scratch/src", do:
```rust
user_doc::load_global_docs_to_path(
  None, None
).expect("must load docs from path");
let docs = &*user_doc::DOCS;
let docs_read_lock = docs.read().expect("must get read guard on global docs");
docs_read_lock.expand_into_mdbook_dirs_at_path(
   DirectoryNamingScheme::ChapterName,
   "tests/scratch/src",
).expect("must expand docs into dirs");
```
The directory `tests/scratch/src` will be filled with documentation corresponding to
[mdbook](https://crates.io/crates/mdbook) format.

## Note
These macros use a temporary directory to persist data from compile-time to runtime.
Do not store sensitive information in doc comments captured with these macros.

License: Apache-2.0 OR MIT
