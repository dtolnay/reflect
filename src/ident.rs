use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct Ident(pub(crate) proc_macro2::Ident);

impl Ident {
    pub fn new<T: Display>(ident: T) -> Self {
        Ident(proc_macro2::Ident::new(
            &ident.to_string(),
            Span::call_site(),
        ))
    }
}

impl Display for Ident {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<proc_macro2::Ident> for Ident {
    fn from(item: proc_macro2::Ident) -> Self {
        Ident(item)
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}
