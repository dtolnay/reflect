use proc_macro2::{self, Span, TokenStream};
use quote::ToTokens;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Ident(proc_macro2::Ident);

impl Ident {
    pub fn new<T: Display>(ident: T) -> Self {
        Ident(proc_macro2::Ident::new(&ident.to_string(), Span::call_site()))
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}
