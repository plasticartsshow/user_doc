use doc_data::*;
use once_cell::sync::{Lazy};
use proc_macro::{TokenStream };
use proc_macro2::{TokenTree, Span};
use quote::quote;
use std::{
  sync::{
    atomic::{AtomicUsize, Ordering},
  },
};
use strum::{AsRefStr, EnumIter, IntoEnumIterator};
use syn::{
  Attribute,
  AttributeArgs,
  Error,
  Field,
  Fields,
  FieldsNamed,
  FieldsUnnamed,
  Ident,
  Item,
  ItemEnum,
  ItemStruct,
  parse_str,
  parse_macro_input,
  Lit,
  LitInt,
  LitStr,
  Meta,
  MetaList,
  MetaNameValue,
  NestedMeta,
  Path,
  parse::{Parse, ParseStream},
  Result,
  Variant,
  Visibility,
};

/// Number of invocations of all procedural macros defined herein
static INVOCATIONS: AtomicUsize = AtomicUsize::new(0);
/// Count an invocation of any procedural macro 
fn count_invocation() -> usize {
  INVOCATIONS.fetch_add(1, Ordering::SeqCst)
}

/// Are two [Path]s likely pointing to roughly the same thing 
fn paths_eq(
  p_0: &Path,
  p_1: &Path
) -> bool {
  format!("{}", quote!{#p_0}) == format!("{}", quote!{#p_1})
}

/// Parsing the outer attributes of any Rust item produces ParsedOuterAttrs.
/// The item is optional. 
#[derive(Clone, Debug)]
struct ParsedOuterAttrs {
  /// Optional visibility modifier 
  pub vis_opt: Option<Visibility>,
  /// Optional outer attributes
  pub outer_attrs: Vec<Attribute>,
  /// The subsequent code item being declared
  pub item_opt: Option<Item>,
}
impl ParsedOuterAttrs{
  /// A default path for docs
  #[allow(clippy::declare_interior_mutable_const)]
  pub const DOC_PATH: Lazy<Path> = Lazy::new(|| parse_str::<Path>("doc").expect("must parse doc path"));
  /// The start of a [syn]-parsed attribute line's string literal segment 
  #[allow(dead_code)]
  pub const DOC_ATTR_LINE_START: &'static str = "= \"";
  /// The end of a [syn]-parsed attribute line's string literal segment 
  pub const DOC_ATTR_LINE_END: &'static str = "\"";
  /// When joining individual doc lines, this will go between adjacent lines
  pub const LINE_JOINER: &'static str = "\n";
  
  /// Extract doc data into the appropriate global record
  pub fn extract_doc_data(
    &self,
    _count: usize,
  ) -> anyhow::Result<()> {
    type AttrsCollectionType = Vec<(Option<Ident>, Vec<(HelperAttr, Attribute)>, Vec<Attribute>)>;
    // let known_helpers = known_helpers_string_vec!();
    let extract_attrs_and_ident_into_map_fold_fn = |
      attrs: &Vec<Attribute>,
      ident: &Option<Ident>,
      m: &mut AttrsCollectionType,
    | -> anyhow::Result<()> {
      let mut helper_attrs_and_attributes 
        : Vec<(HelperAttr, Attribute)> = Vec::with_capacity(attrs.len());
      let mut other_attrs 
        : Vec<Attribute> = Vec::with_capacity(attrs.len());
      for attr in attrs.iter() {
        if let Some(_helper_attr) = HelperAttr::iter()
          .find(|helper_attr| paths_eq(&attr.path, &helper_attr.into())) 
        {
          helper_attrs_and_attributes.push(
            (HelperAttr::from_attribute(attr)?, attr.clone())
          );
        } else {
          other_attrs.push(attr.clone());
        } 
      }
      if !helper_attrs_and_attributes.is_empty() {
        m.push((ident.clone(), helper_attrs_and_attributes, other_attrs));
      }
      Ok(())
    };
    
    let record_doc_from_attrs_collection = |
      attrs_collection: AttrsCollectionType
    | -> anyhow::Result<()> {
      let _attrs_collection_len = attrs_collection.len();
      // std::println!("\nattrs_collection {:?}", attrs_collection );
      for (
        _i, (_ident_opt, helper_attrs_and_attributes, other_attributes)
      ) in attrs_collection.iter().enumerate() {
        // std::println!("\n\nhelper_attrs_and_attributes {:?}", helper_attrs_and_attributes );
        let helper_attrs = helper_attrs_and_attributes.iter().map(|(h,_)| h.clone()).collect();
        // std::println!("\n\nhelper_attrs {:?}", helper_attrs );
        let content: String = Self::get_outer_doc_comments_string(other_attributes);
        
        record_doc_from_helper_attributes_and_str(
          true, //count == INVOCATIONS.load(Ordering::SeqCst) - 1 && i == attrs_collection_len-1,  // throttle saving
          &content, 
          &helper_attrs, 
        )?;
      }
      Ok(()) 
    };
    
    // Extract helper attrs 
    if let Some(ref item) = self.item_opt {
      match item {
        Item::Enum(ItemEnum{ref variants, ..}) => {
          // enum can have helper attrs on its variants, so collect them for each 
          let mut  attrs_collection: AttrsCollectionType = Vec::with_capacity(variants.len());
          for Variant{ref attrs, ref ident, ..} in  variants.iter() {
            extract_attrs_and_ident_into_map_fold_fn( attrs, &Some(ident.clone()), &mut attrs_collection)?
          }
          // std::println!("\n\nattrs {:?}", attrs );

          record_doc_from_attrs_collection(attrs_collection)
        },
        Item::Struct(ItemStruct{/*ref attrs, ref ident,*/ ref fields, ..}) => {
          // struct can have helper attrs on its fields, so collect them for each 
          match fields {
            Fields::Named(FieldsNamed{ ref named, .. }) => {
              let mut  attrs_collection: AttrsCollectionType = Vec::with_capacity(named.len());
              for Field{ref attrs, ref ident, ..} in  named.iter() {
                extract_attrs_and_ident_into_map_fold_fn( attrs, ident, &mut attrs_collection)?
              }
              record_doc_from_attrs_collection(attrs_collection)
            },
            Fields::Unnamed(FieldsUnnamed{ ref unnamed, .. }) => {
              let mut  attrs_collection: AttrsCollectionType = Vec::with_capacity(unnamed.len());
              for Field{ref attrs, ref ident, ..} in  unnamed.iter() {
                extract_attrs_and_ident_into_map_fold_fn( attrs, ident, &mut attrs_collection)?
              }
              record_doc_from_attrs_collection(attrs_collection)
            },
            _ => Ok(())
          }
        },
        _ => {
          Ok(())
        }
      }
    } else {
      Ok(())
    }
  }
  
  #[allow(dead_code)]
  /// Get the quoted text from a doc comment string 
  pub fn extract_comment_text(s: &str) -> String {
    if s.starts_with(Self::DOC_ATTR_LINE_START) && s.ends_with(Self::DOC_ATTR_LINE_END) {
      s[Self::DOC_ATTR_LINE_START.len()..(s.len() - Self::DOC_ATTR_LINE_END.len())].to_string()
    } else {
      String::new()
    }
  }
  
  #[allow(clippy::borrow_interior_mutable_const)]
  /// Get a collection of the lines of doc comments 
  pub fn get_doc_comments_lines(
    outer_attrs: &[Attribute]
  ) -> Vec<String> {
    outer_attrs.iter()
    .filter_map(|Attribute{path, ref tokens, ..}| {
      if paths_eq(path, &*Self::DOC_PATH) {
        match tokens.clone().into_iter().nth(1) {
          Some(TokenTree::Literal(literal)) => {
            Some(
              literal
                .to_string()
                .trim_end_matches(Self::DOC_ATTR_LINE_END)
                .trim_start_matches(Self::DOC_ATTR_LINE_END)
                .to_string()
            )
          },
          _ => None
        }
      } else {
        None 
      }
    })
      .collect()
  }

  /// Get a catenated string with all the doc comments 
  pub fn get_outer_doc_comments_string(attrs: &[Attribute]) -> String {
    Self::get_doc_comments_lines(attrs).join(ParsedOuterAttrs::LINE_JOINER)
  }
}
impl Parse for ParsedOuterAttrs {
  fn parse(input: ParseStream) -> Result<Self> {
    // parse the outer attributes 
    Ok(Self{
      outer_attrs: input.call(Attribute::parse_outer)?,
      vis_opt: input.parse().ok(),
      item_opt: input.parse().ok()
    })
  }
}

#[allow(clippy::enum_variant_names)]
#[derive(AsRefStr, Clone, EnumIter, Debug, Eq, Ord, PartialOrd, PartialEq)]
#[strum(serialize_all = "snake_case")]
/// Attribute helpers
enum HelperAttr {
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
    if paths_eq(path, &(&Self::ChapterNum(0)).into()){
      let chapter_num_lit_int = a.parse_args::<LitInt>()?;
      Ok(Self::ChapterNum(chapter_num_lit_int.base10_parse()?))
    } else if paths_eq(path, &(&Self::ChapterName(String::new())).into()){
      let chapter_name_lit_str = a.parse_args::<LitStr>()?;
      Ok(Self::ChapterName(chapter_name_lit_str.value()))
    } else if paths_eq(path, &(&Self::ChapterNameSlug(Vec::new())).into()){
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
    } else if paths_eq(path, &(&Self::ChapterNumSlug(Vec::new())).into()){
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

#[proc_macro_derive(
  user_doc_item,
  attributes( 
    chapter_name,
    chapter_name_slug,
    chapter_num, 
    chapter_num_slug,
  )
)]
/// Use this attribute macro to define user-facing documentation on a non-function item..
pub fn user_doc_item(
  item: TokenStream
) -> TokenStream {
  let count = count_invocation();
  // let span: Span = item.span();
  let parsed_outer_attrs = parse_macro_input!(item as ParsedOuterAttrs);
  // std::println!("parsed item outer attrs: {:?}", parsed_outer_attrs );
  // std::println!("parsed user_doc_item derive");
  match parsed_outer_attrs.extract_doc_data(count) {
    Ok(()) => TokenStream::new(),
    Err(extraction_error) => Error::new(
      Span::call_site(),
      format!(
        "Could not extract doc data during derive macro invocation:\n{:#?}",
        extraction_error
      )
    ).into_compile_error().into()
  } 
}

#[proc_macro_attribute]
/// Use this attribute macro to define user-facing documentation on a function item.
pub fn user_doc_fn(
  own_attr: TokenStream,
  item: TokenStream,
) -> TokenStream {
  let _count:usize = count_invocation();
  // Copy item for output 
  let it = item.clone();
  // Capture own helper attributes 
  let own_attribute_args = parse_macro_input!(own_attr as AttributeArgs);
  let helper_attributes_res: Result<Vec<HelperAttr>> = HelperAttr::from_attribute_args(&own_attribute_args);
  let parsed_outer_attrs = parse_macro_input!(item as ParsedOuterAttrs);
  // std::println!("parsed user_doc_fn attribute");
  // A fn can only generate a single Documentable::Doc item
  // std::println!("parsed item outer attrs: {:?}", parsed_outer_attrs.get_doc_comments_lines() );
  match helper_attributes_res {
    Ok(helper_attributes) => {
      match record_doc_from_helper_attributes_and_str(
        true, // count == INVOCATIONS.load(Ordering::SeqCst) - 1, // Throttle saving to most recent invocation
        &ParsedOuterAttrs::get_outer_doc_comments_string(&parsed_outer_attrs.outer_attrs),
        &helper_attributes, 
      ) {
        Ok(()) => it,
        Err(err) => err.into_compile_error().into()
      }
    },
    Err(err) => err.into_compile_error().into()
  }
}

#[allow(clippy::ptr_arg)]
/// Record a Documentable specified by the given [HelperAttr]s and [str] to the global store.
fn record_doc_from_helper_attributes_and_str(
  do_save: bool,
  doc_comment_string: &str,
  helper_attributes: &Vec<HelperAttr>,
) -> Result<()> {
  // std::println!("helper_attributes {:#?}", helper_attributes );
  // Decide where to insert the generated doc 
  let mut name_opt = None;
  let mut number_opt = None;
  let mut name_path_opt = None;
  let mut number_path_opt = None;
  for helper_attribute in helper_attributes.iter() {
    match helper_attribute {
      HelperAttr::ChapterName(ref chapter_name) => {
        name_opt = Some(chapter_name.to_string());
      },
      HelperAttr::ChapterNameSlug(ref chapter_names) => {
        name_path_opt = Some(chapter_names.to_vec());
      },
      HelperAttr::ChapterNum(ref chapter_number) => {
        number_opt = Some(*chapter_number);
      },
      HelperAttr::ChapterNumSlug(ref chapter_numbers) => {
        number_path_opt = Some(chapter_numbers.to_vec());
      },          
    }
  }
  // std::println!("name_opt {:?}, \n\tnumber_opt {:?}, \n\tname_path_opt {:?}, \n\tnumber_path_opt {:?}", name_opt, number_opt, name_path_opt, number_path_opt );
  // Generate the new doc record 
  let generate_documentable = |
    name_opt: &Option<String> 
  | -> Documentable {
    let documentable = Documentable::Doc(
      name_opt.as_ref().cloned().unwrap_or_default(),
      doc_comment_string.to_string()
    );
    // std::println!("generated documentable {}", documentable );
    documentable
  };
  // Prepare and write to the insertion location in the global store
  let docs = &*doc_data::DOCS;
  let mut docs_write_lock = docs.write().expect("Must get write lock on global docs");
  let write_res = match (number_path_opt, name_path_opt) {
    (Some(ref path_numbers), Some(ref path_names)) => {
      // Update name path from last slug segments
      if name_opt.is_none() {
        name_opt = path_names.get(path_numbers.len()-1).cloned();
      }
      // Create the documentable to insert
      let documentable = generate_documentable(&name_opt);
      // std::println!("writing {}  {:?}", documentable, std::time::Instant::now(), );
      // insert at given numberpath and namepath combo
      docs_write_lock.add_path(
        &name_opt,
        Some(documentable),
        Some(true),
        path_names,
        path_numbers,
      )
    },
    (Some(ref path_numbers), None) => {
      let documentable = generate_documentable(&name_opt);
      // std::println!("writing {}  {:?}", documentable, std::time::Instant::now(), );
      // insert at given numberpath with default empty names 
      docs_write_lock.add_path(
        &name_opt,
        Some(documentable),
        Some(true),
        &[],
        path_numbers,
      )
    },
    (None, Some(ref path_names)) => {
      // update name path from last slug segment
      if name_opt.is_none() {
        name_opt = path_names.last().cloned();
      }
      let documentable = generate_documentable(&name_opt);
      // std::println!("writing {}  {:?}", documentable, std::time::Instant::now(), );
      // insert at given namepath with default autogenerated numbers 
      docs_write_lock.add_path(
        &name_opt,
        Some(documentable),
        Some(true),
        path_names,
        &[],
      )
    },
    (None, None) => {
      // insert at autogenerated number path with default empty name
      let documentable = generate_documentable(&name_opt);
      // std::println!("writing {}  {:?}", documentable, std::time::Instant::now(), );
      docs_write_lock.add_entry(
        documentable,
        name_opt,
        number_opt,
        Some(true),
      )
    }
  };
  // std::println!("docs_write_lock {:?} {:?}", std::time::Instant::now(), *docs_write_lock );
  drop(docs_write_lock);
  match write_res {
    Ok(()) => {
      if do_save {
        // std::println!("saving to default path and file", );
        match doc_data::save_docs_to_path(
          None, 
          None,
        ) {
          Ok(()) => {
            Ok(())
          }
          Err(doc_save_error) => {
            Err(Error::new(
              Span::mixed_site(),
              format!("{:#?}", doc_save_error)
            ))
          }
        }
      } else {
        Ok(())
      }
    },
    Err(error) => {
      Err(Error::new(
        Span::mixed_site(),
        format!("{:#?}", error),
      ))
    }
  }
}
