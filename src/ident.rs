use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new<T: Display>(ident: T) -> Self {
        Ident(ident.to_string())
    }
}

impl Display for Ident {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        proc_macro2::Ident::new(&self.0, Span::call_site()).fmt(f)
    }
}

impl From<proc_macro2::Ident> for Ident {
    fn from(item: proc_macro2::Ident) -> Self {
        Ident(item.to_string())
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        proc_macro2::Ident::new(&self.0, Span::call_site()).to_tokens(tokens);
    }
}
