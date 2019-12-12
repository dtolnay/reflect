use crate::{GenericArgument, GenericArguments, Ident, ParamMap, Type};
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_str, ReturnType, Token};

#[derive(Debug, Clone)]
pub struct Path {
    pub(crate) global: bool,
    pub(crate) path: Vec<PathSegment>,
}

pub(crate) struct SimplePath {
    pub(crate) global: bool,
    pub(crate) path: Vec<Ident>,
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

    /// Get a simple path without generics
    pub(crate) fn get_path(&self, ident: &str) -> Self {
        let mut path = self.clone();
        path.path.push(PathSegment {
            ident: Ident::from(
                parse_str::<syn::Ident>(ident).expect("Path::get_path: Not an Ident"),
            ),
            args: PathArguments::None,
        });
        path
    }

    pub fn simple_path_from_str(path: &str) -> Self {
        parse_str::<SimplePath>(path)
            .expect("simple_path_from_str: Not a simple path")
            .into()
    }

    pub(crate) fn set_params(&mut self, params: &[&str], param_map: &mut ParamMap) {
        use PathArguments::*;
        let last_index = self.path.len() - 1;
        let mut last_segment = &mut self.path[last_index];

        match last_segment.args {
            None => {
                last_segment.args = AngleBracketed(AngleBracketedGenericArguments {
                    args: GenericArguments {
                        args: params
                            .iter()
                            .map(|param| {
                                GenericArgument::syn_to_generic_argument(
                                    parse_str(param).unwrap(),
                                    param_map,
                                )
                            })
                            .collect(),
                    },
                })
            }
            AngleBracketed(AngleBracketedGenericArguments { ref mut args }) => {
                args.args.extend(params.iter().map(|param| {
                    GenericArgument::syn_to_generic_argument(parse_str(param).unwrap(), param_map)
                }))
            }
            _ => panic!("Path::add_params: ParenthesizedGenericArguments"),
        }
    }

    pub(crate) fn syn_to_path(path: syn::Path, param_map: &mut ParamMap) -> Self {
        match path {
            syn::Path {
                leading_colon,
                segments,
            } => {
                let path: Vec<_> = segments
                    .into_iter()
                    .map(|segment| Self::syn_to_path_segment(segment, param_map))
                    .collect();
                Path {
                    global: leading_colon.is_some(),
                    path,
                }
            }
        }
    }

    pub(crate) fn syn_to_path_segment(
        path_segment: syn::PathSegment,
        param_map: &mut ParamMap,
    ) -> PathSegment {
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
                            .map(|arg| GenericArgument::syn_to_generic_argument(arg, param_map))
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
                        .map(|input| Type::syn_to_type(input, param_map))
                        .collect(),
                    output: match parenthesized.output {
                        ReturnType::Default => None,
                        ReturnType::Type(_, ty) => Some(Type::syn_to_type(*ty, param_map)),
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

impl From<SimplePath> for Path {
    fn from(simple: SimplePath) -> Self {
        Path {
            global: simple.global,
            path: simple
                .path
                .into_iter()
                .map(|ident| PathSegment {
                    ident,
                    args: PathArguments::None,
                })
                .collect(),
        }
    }
}

impl Parse for SimplePath {
    fn parse(input: ParseStream) -> Result<Self> {
        let global = input.parse::<Option<Token![::]>>()?.is_some();
        let first = Ident::from(input.parse::<syn::Ident>()?);
        let mut path = vec![first];
        while !input.is_empty() {
            input.parse::<Token![::]>()?;
            path.push(Ident::from(input.parse::<syn::Ident>()?));
        }
        Ok(SimplePath { global, path })
    }
}
