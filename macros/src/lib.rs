extern crate proc_macro;

use self::proc_macro::TokenStream;

#[proc_macro]
pub fn library(input: TokenStream) -> TokenStream {
    let _ = input;
    unimplemented!()
}
