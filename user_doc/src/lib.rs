//! Why define documentation in multiple places?  
//!
//! The (attribute) [macro@user_doc_fn] and (derive) [macro@user_doc_item] macros 
//! capture documentation from comments and make said documentation
//! available at runtime. 
//! 
//! ---
//! 
//! # How to use it on a function definition:  
//! ```ignore
//! #[user_doc_fn(
//!   chapter_num_slug(1, 3, 5),
//!   chapter_name_slug(
//!     "A Slaying in Luton", 
//!     "The Trouble About Ipswich",
//!     "All Along the Weary M-5 Motorway",
//!   ),
//! )]
//! /// The parenchyma isn't as stiff as usual. It looks almost floppy.
//! /// I stick out a hand to touch it. It sucks my fingertips forward.
//! /// When I pull my hand back, a hanging bridge of sap follows. 
//! pub fn call_this_function() -> bool { true }
//! ``` 
//! 
//! The commented lines (from "The parenchyma" to "sap follows.") will be 
//! captured and assigned a location in a tree hierarchy:  
//! Chapters: 1. "A Slaying in Luton" > 3. "The Trouble About Ipswich" > 5. "All Along the Weary M-5 Motorway"  
//! So that at runtime:  
//! ```ignore
//! doc_data::load_global_docs_to_path(
//!   None, None
//! ).expect("must load docs from path");
//! let docs = &*doc_data::DOCS;
//! let docs_read_lock = docs.read().expect("must get read guard on global docs");
//! assert_eq!(
//!   docs_read_lock.get_entry_at_numeric_path(&[1,3,5]).expect("must find test entry").1,
//!   " The parenchyma isn\\'t as stiff as usual. It looks almost floppy.\
//!     \n I stick out a hand to touch it. It sucks my fingertips forward.\
//!     \n When I pull my hand back, a hanging bridge of sap follows.",
//! );
//! ``` 
//!
//! The doc comment has been inserted in a tree structure. Wow.
//!
//! ## How to use it on a struct or enum definition.
//! When working with structs or enums, the outer attribute commentss are NOT captured:
//!
//! ```ignore
//! #[derive(user_doc_item, Clone, Debug, PartialEq, Eq)]
//! /// This comment WILL NOT BE captured for user docs.
//! pub enum Idiot {
//!   #[chapter_num(23)] // should be overriden by slug 
//!   #[chapter_num_slug(1, 3, 4)]
//!   #[chapter_name("The House of Almond Blossoms")]
//!   /// He took a look at the card I showed him. His brows scrunched up like he smelled something offensive. 
//!   /// Could he tell it was fake? Everyone sweats in heat like this. If they don't, it means they're about to keel over from dehydration anyway. Still, I felt like I was sweating more than usual.
//!   /// We stood frozen for a moment. It seemed an eon. Then he mercifully broke the silence. 
//!   /// "I can't read those. I'm just supposed to stand here and not let anyone pass."
//!   /// I considered. I didn't want to get him in trouble – he was clearly new. Still, it was me or him.
//!   /// "Oh," I said as casually as I could, "It says you're to let me through. But don't let anyone else through after me. It says that too.""
//!   Kid(u16),
//! }
//! let docs = &*user_doc::DOCS;
//! let docs_read_lock = docs.read().expect("must get read guard on global docs");
//! std::println!("docs_read_lock {:?} {:#?}", std::time::Instant::now(), *docs_read_lock );
//! assert_eq!(
//!   docs_read_lock.get_entry_at_numeric_path(&[1,3,4]).expect("must find test entry").0,
//!   String::from("The House of Almond Blossoms"),
//! );
//! ```
//! Here, the `chapter_num_slug` helper attribute has overriden the `chapter_num` attribute. 
//! Slug-style (number or name) attributes and arguments will always take precedence.
//!
//! # How to use it with `mdbook`
//!
//! Call the [doc_data::DocDict::expand_into_mdbook_dirs_at_path()] method on a DocDict generated 
//! from doc comments. This will extract the comments from the DocDict into a directory structure 
//! that can be used with mdbook.
//!
//! ### For instance:
//! To expand the global doc store into a hierarchy at the path "tests/scratch/src", do:
//! ```ignore
//! user_doc::load_global_docs_to_path(
//!   None, None
//! ).expect("must load docs from path");
//! let docs = &*user_doc::DOCS;
//! let docs_read_lock = docs.read().expect("must get read guard on global docs");
//! docs_read_lock.expand_into_mdbook_dirs_at_path(
//!    DirectoryNamingScheme::ChapterName,
//!    "tests/scratch/src",
//! ).expect("must expand docs into dirs");
//! ```


extern crate doc_proc_macro;
pub use doc_proc_macro::*;

extern crate doc_data;
pub use doc_data::*;
