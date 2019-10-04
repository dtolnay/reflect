use crate::Ident;
use crate::Type;
use crate::TypeNode;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use ref_cast::RefCast;

#[derive(RefCast)]
#[repr(C)]
pub(crate) struct Print<T>(T);

impl ToTokens for Print<Type> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Print::ref_cast(&(self.0).0).to_tokens(tokens);
    }
}

impl ToTokens for Print<TypeNode> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use crate::TypeNode::*;
        tokens.append_all(match self.0 {
            Infer => quote!(_),
            Unit => quote!(()),
            PrimitiveStr => quote!(str),
            Reference(ref inner) => {
                let inner = Print::ref_cast(&**inner);
                quote!(&#inner)
            }
            ReferenceMut(ref inner) => {
                let inner = Print::ref_cast(&**inner);
                quote!(&mut #inner)
            }
            Dereference(ref inner) => panic!("Type::Dereference::to_tokens"),
            DataStructure { ref name, .. } => {
                // FIXME generics
                let name = Ident::from(name.clone());
                quote!(#name)
            }
            Path {
                global,
                ref path,
                ref name,
                ..
            } => {
                // FIXME generics
                let leading = if global { Some(quote!(::)) } else { None };
                let path = path.iter().map(Ident::new);
                let name = Ident::new(name);
                quote!(#leading #(#path ::)* #name)
            }
        });
    }
}
