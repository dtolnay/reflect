use crate::Ident;
use crate::Path;
use crate::Type;
use crate::TypeNode;

//FIXME: where clause

#[derive(Debug, Clone)]
pub struct Generics {
    pub(crate) params: Vec<GenericParam>,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericParam {
    Type(PredicateType),
    Lifetime(LifetimeDef),
    Const(ConstParam),
}

#[derive(Debug, Clone)]
pub(crate) struct PredicateType {
    pub(crate) lifetimes: Vec<Lifetime>,
    pub(crate) bounded_ty: Type,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone)]
pub(crate) enum TypeParamBound {
    Trait(TraitBound),
    Lifetime(Lifetime),
}

#[derive(Debug, Clone)]
pub(crate) struct TraitBound {
    ///A set of bound Lifetimes: `for<'a, 'b, 'c>`.
    pub(crate) lifetimes: Vec<Lifetime>,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct Lifetime {
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone)]
pub(crate) struct LifetimeDef {
    pub(crate) ident: Ident,
    pub(crate) bounds: Vec<Lifetime>,
}

#[derive(Debug, Clone)]
pub(crate) struct ConstParam {
    pub(crate) private: (),
}

#[derive(Debug, Clone)]
pub struct GenericArguments {
    pub(crate) args: Vec<GenericArgument>,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericArgument {
    Lifetime(Lifetime),
    Type(Type),
    Binding(Binding),
    Constraint(Constraint),
    Const(Expr),
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) ident: Ident,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct Constraint {
    pub(crate) ident: Ident,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub(crate) private: (),
}

impl Generics {
    pub(crate) fn syn_to_generics(generics: syn::Generics) -> Generics {
        // FIXME where clause
        Generics {
            params: generics
                .params
                .into_iter()
                .map(|param| match param {
                    syn::GenericParam::Type(syn::TypeParam { ident, bounds, .. }) => {
                        GenericParam::Type(PredicateType {
                            lifetimes: Vec::new(),
                            bounded_ty: Type(TypeNode::Path(Path::ident_to_path(Ident::from(ident)))),
                            bounds: bounds
                                .into_iter()
                                .map(|bound| match bound {
                                    syn::TypeParamBound::Lifetime(syn::Lifetime {
                                        ident, ..
                                    }) => TypeParamBound::Lifetime(Lifetime {
                                        ident: Ident::from(ident),
                                    }),

                                    syn::TypeParamBound::Trait(syn::TraitBound {
                                        lifetimes,
                                        path,
                                        ..
                                    }) => TypeParamBound::Trait(TraitBound {
                                        lifetimes: lifetimes
                                            .map_or_else(Vec::new, |lifetimes| {
                                                lifetimes.lifetimes.into_iter().map(
                                                    |syn::LifetimeDef { lifetime: syn::Lifetime {ident, ..}, ..}| Lifetime {
                                                        ident: Ident::from(ident),
                                                    },
                                                ).collect()
                                            }),
                                        ty: Type(TypeNode::Path(Path::syn_to_path(path))),
                                    }),
                                })
                                .collect(),
                        })
                    }
                    syn::GenericParam::Lifetime(syn::LifetimeDef { lifetime: syn::Lifetime {ident, ..}, bounds, ..}) => {
                        GenericParam::Lifetime(LifetimeDef {
                            ident: Ident::from(ident),
                            bounds: bounds.into_iter().map(|syn::Lifetime {ident, ..}| {
                                Lifetime {
                                    ident: Ident::from(ident),
                                }
                            }).collect(),
                        })
                    },
                    syn::GenericParam::Const(_const) => {
                        unimplemented!("Generics::syn_to_generics: Const")
                    }
                })
                .collect(),
        }
    }
}
