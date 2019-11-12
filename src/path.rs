use crate::{GenericArgument, GenericArguments, Ident, Type};
use std::str::FromStr;
use syn::parse::Result;
use syn::{parse_str, Error, ReturnType};

#[derive(Debug, Clone)]
// Consider just using syn::Path
pub struct Path {
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
    pub(crate) output: Option<Type>,
}

impl Path {
    pub(crate) fn root() -> Self {
        Path {
            global: true,
            path: Vec::new(),
        }
    }

    pub(crate) fn empty() -> Self {
        Path {
            global: false,
            path: Vec::new(),
        }
    }

    pub(crate) fn get_path(&self, segment: &str) -> Self {
        let mut path = self.clone();
        path.path
            .push(Self::syn_to_path_segment(parse_str(segment).unwrap()));
        path
    }

    pub(crate) fn syn_to_path(path: syn::Path) -> Self {
        match path {
            syn::Path {
                leading_colon,
                segments,
            } => {
                let path: Vec<_> = segments
                    .into_iter()
                    .map(Self::syn_to_path_segment)
                    .collect();
                Path {
                    global: leading_colon.is_some(),
                    path,
                }
            }
        }
    }

    pub(crate) fn syn_to_path_segment(path_segment: syn::PathSegment) -> PathSegment {
        let syn::PathSegment { ident, arguments } = path_segment;
        let ident = Ident::from(ident);
        match arguments {
            syn::PathArguments::None => PathSegment {
                ident,
                args: PathArguments::None,
            },
            syn::PathArguments::AngleBracketed(generic_args) => PathSegment {
                ident,
                args: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args: GenericArguments {
                        args: generic_args
                            .args
                            .into_iter()
                            .map(GenericArgument::syn_to_generic_argument)
                            .collect(),
                    },
                }),
            },

            syn::PathArguments::Parenthesized(parenthesized) => PathSegment {
                ident,
                args: PathArguments::Parenthesized(ParenthesizedGenericArguments {
                    inputs: parenthesized
                        .inputs
                        .into_iter()
                        .map(Type::syn_to_type)
                        .collect(),
                    output: match parenthesized.output {
                        ReturnType::Default => None,
                        ReturnType::Type(_, ty) => Some(Type::syn_to_type(*ty)),
                    },
                }),
            },
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

impl FromStr for Path {
    type Err = Error;
    // TODO: Consider using custum error type
    fn from_str(path: &str) -> Result<Path> {
        Ok(Path::syn_to_path(parse_str(path)?))
    }
}
