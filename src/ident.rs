use proc_macro2::{Span, Term};
use quote::{ToTokens, Tokens};
use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Ident(Term);

impl Ident {
    pub fn new<T: Display>(ident: T) -> Self {
        Ident(Term::new(&ident.to_string(), Span::call_site()))
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut Tokens) {
        self.0.to_tokens(tokens);
    }
}
