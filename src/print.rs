use crate::generics::*;
use crate::{path, Accessor, Type, TypeNode};
use proc_macro2::{Punct, Spacing, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use ref_cast::RefCast;
use syn::LitInt;

#[derive(RefCast)]
#[repr(C)]
pub(crate) struct Print<T>(T);

impl ToTokens for Print<Accessor> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use Accessor::*;
        match &self.0 {
            Name(ident) => ident.to_tokens(tokens),
            Index(i) => LitInt::new(&i.to_string(), Span::call_site()).to_tokens(tokens),
        }
    }
}

impl ToTokens for Print<Type> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Print::ref_cast(&(self.0).0).to_tokens(tokens);
    }
}

impl ToTokens for Print<TypeNode> {
    //FIXME: generics
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use crate::TypeNode::*;
        tokens.append_all(match &self.0 {
            Infer => quote!(_),
            Tuple(types) => {
                if types.len() == 1 {
                    let ty = Print::ref_cast(&types[0]);
                    quote!((#ty,))
                } else {
                    let types = types.iter().map(Print::ref_cast);
                    quote!((#(#types),*))
                }
            }
            PrimitiveStr => quote!(str),
            Reference { lifetime, inner } => {
                let lifetime = lifetime.as_ref().map(|lifetime| Print::ref_cast(lifetime));
                let inner = Print::ref_cast(&**inner);
                quote!(&#lifetime #inner)
            }
            ReferenceMut { lifetime, inner } => {
                let lifetime = lifetime.as_ref().map(|lifetime| Print::ref_cast(lifetime));
                let inner = Print::ref_cast(&**inner);
                quote!(&mut #lifetime #inner)
            }
            Dereference(inner) => panic!("Type::Dereference::to_tokens"),
            DataStructure { name, .. } => {
                //FIXME: generics
                quote!(#name)
            }
            TraitObject(bounds) => {
                let bounds = bounds.iter().map(Print::ref_cast);
                quote!((dyn #(#bounds)+*))
            }
            Path(path) => {
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
        match &self.0 {
            GenericParam::Type(type_param) => Print::ref_cast(type_param).to_tokens(tokens),
            GenericParam::Lifetime(lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),
            GenericParam::Const(_const) => unimplemented!("const generics"),
        }
    }
}

impl ToTokens for Print<GenericConstraint> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self.0 {
            GenericConstraint::Type(predicate) => Print::ref_cast(predicate).to_tokens(tokens),
            GenericConstraint::Lifetime(lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),
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
        match &self.0 {
            TypeParamBound::Trait(trait_bound) => Print::ref_cast(trait_bound).to_tokens(tokens),

            TypeParamBound::Lifetime(lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),
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
        tokens.append_all(quote!(#lifetimes #ty #colon #(#bounds)+*))
    }
}

impl ToTokens for Print<Lifetime> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let apostrophe = Punct::new('\'', Spacing::Joint);
        tokens.append(apostrophe);
        self.0.ident.to_tokens(tokens);
    }
}

impl ToTokens for Print<LifetimeDef> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let apostrophe = Punct::new('\'', Spacing::Joint);
        tokens.append(apostrophe);
        let ident = &self.0.ident;
        let bounds = self.0.bounds.iter().map(Print::ref_cast);
        let colon = if self.0.bounds.is_empty() {
            None
        } else {
            Some(quote!(:))
        };
        tokens.append_all(quote!(#ident #colon #(#bounds)+*))
    }
}

impl ToTokens for Print<Binding> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.0.ident;
        let ty = Print::ref_cast(&self.0.ty);
        tokens.append_all(quote!(#ident : ty))
    }
}

impl ToTokens for Print<Constraint> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.0.ident;
        let bounds = self.0.bounds.iter().map(Print::ref_cast);
        let colon = if self.0.bounds.is_empty() {
            None
        } else {
            Some(quote!(:))
        };
        tokens.append_all(quote!(#ident #colon #(#bounds)+*))
    }
}

impl ToTokens for Print<GenericArgument> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self.0 {
            GenericArgument::Lifetime(lifetime) => Print::ref_cast(lifetime).to_tokens(tokens),

            GenericArgument::Type(ty) => Print::ref_cast(ty).to_tokens(tokens),

            GenericArgument::Binding(binding) => Print::ref_cast(binding).to_tokens(tokens),

            GenericArgument::Constraint(constraint) => {
                Print::ref_cast(constraint).to_tokens(tokens)
            }

            GenericArgument::Const(_expr) => unimplemented!(),
        }
    }
}

impl ToTokens for Print<path::Path> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use path::PathArguments;
        let leading = if self.0.global {
            Some(quote!(::))
        } else {
            None
        };
        let path = self.0.path.iter().map(|segment| {
            let ident = &segment.ident;
            let args = match &segment.args {
                PathArguments::None => None,
                PathArguments::AngleBracketed(args) => {
                    let args = args.args.args.iter().map(Print::ref_cast);
                    Some(quote!(<#(#args),*>))
                }
                PathArguments::Parenthesized(args) => {
                    let inputs = args.inputs.iter().map(Print::ref_cast);
                    let output = args.output.as_ref().map(|output| {
                        let output = Print::ref_cast(output);
                        quote! {-> output}
                    });
                    Some(quote! { (#(#inputs),*) #output})
                }
            };
            quote!(#ident #args)
        });
        tokens.append_all(quote!(#leading #(#path)::*));
    }
}
