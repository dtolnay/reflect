use crate::generics::*;
use crate::path;
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
    //FIXME: generics
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use crate::TypeNode::*;
        tokens.append_all(match self.0 {
            Infer => quote!(_),
            Unit => quote!(()),
            PrimitiveStr => quote!(str),
            Reference {
                ref lifetime,
                ref inner,
            } => {
                let lifetime = lifetime
                    .as_ref()
                    .map(|lifetime| Ident::new(format!("'{}", lifetime.ident)));
                let inner = Print::ref_cast(&**inner);
                quote!(&#lifetime #inner)
            }
            ReferenceMut {
                ref lifetime,
                ref inner,
            } => {
                let lifetime = lifetime
                    .as_ref()
                    .map(|lifetime| Ident::new(format!("'{}", lifetime.ident)));
                let inner = Print::ref_cast(&**inner);
                quote!(&mut #lifetime #inner)
            }
            Dereference(ref inner) => panic!("Type::Dereference::to_tokens"),
            DataStructure { ref name, .. } => {
                //FIXME: generics
                let name = Ident::from(name.clone());
                quote!(#name)
            }
            TraitObject(ref bounds) => {
                let bounds = bounds.iter().map(Print::ref_cast);
                quote!((dyn #(#bounds)+*))
            }
            Path(ref path) => {
                let path = Print::ref_cast(path);
                quote!(#path)
            }
        });
    }
}

impl ToTokens for Print<Generics> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let params = self.0.params.iter().map(Print::ref_cast);
        tokens.append_all(quote!(#(#params),*))
    }
}

impl ToTokens for Print<GenericParam> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.0 {
            GenericParam::Type(ref type_param) => Print::ref_cast(type_param).to_tokens(tokens),
            GenericParam::Lifetime(ref lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),
            GenericParam::Const(ref _const) => unimplemented!("const generics"),
        }
    }
}

impl ToTokens for Print<TypeParam> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.0.ident;
        ident.to_tokens(tokens);
    }
}

impl ToTokens for Print<TypeParamBound> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.0 {
            TypeParamBound::Trait(ref trait_bound) => {
                Print::ref_cast(trait_bound).to_tokens(tokens)
            }

            TypeParamBound::Lifetime(ref lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),
        }
    }
}

impl ToTokens for Print<TraitBound> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let path = Print::ref_cast(&self.0.path);
        let lifetimes = self.0.lifetimes.iter().map(Print::ref_cast);
        let lifetimes = if self.0.lifetimes.is_empty() {
            None
        } else {
            Some(quote!(for <#(#lifetimes)+*>))
        };
        tokens.append_all(quote!(#lifetimes #path))
    }
}

impl ToTokens for Print<PredicateType> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let lifetimes = self.0.lifetimes.iter().map(Print::ref_cast);
        let lifetimes = if self.0.lifetimes.is_empty() {
            None
        } else {
            Some(quote!(for <#(#lifetimes)+*>))
        };
        let ty = Print::ref_cast(&self.0.bounded_ty);
        let bounds = self.0.bounds.iter().map(Print::ref_cast);
        let colon = if self.0.bounds.is_empty() {
            None
        } else {
            Some(quote!(:))
        };
        tokens.append_all(quote!(#lifetimes #ty #colon (#(#bounds)+*)))
    }
}

impl ToTokens for Print<Lifetime> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::new(format!("'{}", self.0.ident));
        tokens.append_all(quote!(#ident))
    }
}

impl ToTokens for Print<LifetimeDef> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::new(format!("'{}", self.0.ident));
        let bounds = self.0.bounds.iter().map(Print::ref_cast);
        let colon = if self.0.bounds.is_empty() {
            None
        } else {
            Some(quote!(:))
        };
        tokens.append_all(quote!(#ident #colon (#(#bounds)+*)))
    }
}

impl ToTokens for Print<Binding> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::from(self.0.ident.clone());
        let ty = Print::ref_cast(&self.0.ty);
        tokens.append_all(quote!(#ident : ty))
    }
}

impl ToTokens for Print<Constraint> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = Ident::from(self.0.ident.clone());
        let bounds = self.0.bounds.iter().map(Print::ref_cast);
        let colon = if self.0.bounds.is_empty() {
            None
        } else {
            Some(quote!(:))
        };
        tokens.append_all(quote!(#ident #colon (#(#bounds)+*)))
    }
}

impl ToTokens for Print<GenericArgument> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.0 {
            GenericArgument::Lifetime(ref lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),

            GenericArgument::Type(ref ty) => Print::ref_cast(ty).to_tokens(tokens),

            GenericArgument::Binding(ref binding) => Print::ref_cast(binding).to_tokens(tokens),

            GenericArgument::Constraint(ref constraint) => {
                Print::ref_cast(constraint).to_tokens(tokens)
            }

            GenericArgument::Const(ref _expr) => unimplemented!(),
        }
    }
}

impl ToTokens for Print<path::Path> {
    //FIXME generics
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let leading = if self.0.global {
            Some(quote!(::))
        } else {
            None
        };
        let path = self.0.path.iter().map(|segment| &segment.ident);
        tokens.append_all(quote!(#leading #(#path)::*));
    }
}
