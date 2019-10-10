use crate::GenericArguments;
use crate::Ident;
use crate::Type;

#[derive(Debug, Clone)]
pub(crate) struct Path {
    pub(crate) global: bool,
    pub(crate) path: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub(crate) struct PathSegment {
    pub(crate) ident: Ident,
    pub(crate) args: PathArguments,
}

#[derive(Debug, Clone)]
pub(crate) enum PathArguments {
    None,
    AngleBracketed(AngleBracketedGenericArguments),
    Parenthesized(ParenthesizedGenericArguments),
}

#[derive(Debug, Clone)]
pub(crate) struct AngleBracketedGenericArguments {
    pub(crate) args: GenericArguments,
}

/// Arguments of a function path segment: the `(A, B) -> C` in `Fn(A, B) -> C`.
#[derive(Debug, Clone)]
pub(crate) struct ParenthesizedGenericArguments {
    /// (A, B)
    pub(crate) inputs: Vec<Type>,
    /// C
    pub(crate) output: Type,
}

impl Path {
    pub(crate) fn syn_to_path(path: syn::Path) -> Path {
        match path {
            syn::Path {
                leading_colon,
                segments,
            } => {
                let path: Vec<_> = segments
                    .into_iter()
                    .map(|syn::PathSegment { ident, arguments }| {
                        let ident = Ident::from(ident);
                        match arguments {
                            syn::PathArguments::None => PathSegment {
                                ident,
                                args: PathArguments::None,
                            },
                            //FIXME: generics
                            syn::PathArguments::AngleBracketed(_generic_args) => {
                                unimplemented!("Type::syn_to_type: angle bracketed generic args")
                            }

                            //FIXME: Generics
                            syn::PathArguments::Parenthesized(_parenthesized) => {
                                unimplemented!("Type::syn_to_type: parentesized generic args")
                            }
                        }
                    })
                    .collect();
                Path {
                    global: leading_colon.is_some(),
                    path,
                }
            }
        }
    }

    pub(crate) fn ident_to_path(ident: Ident) -> Path {
        Path {
            global: false,
            path: vec![PathSegment {
                ident,
                args: PathArguments::None,
            }],
        }
    }
}
