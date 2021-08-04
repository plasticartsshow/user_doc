use proc_macro::{
  TokenStream 
};
use proc_macro2::{
  TokenStream as TokenStream2
};
use doc_data::*;


#[proc_macro_attribute]
pub fn user_doc(
  attr: TokenStream,
  item: TokenStream
) -> TokenStream {
  std::println!("attr {}", attr );
  std::println!("item {}", item );
  item
}
