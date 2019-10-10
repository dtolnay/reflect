use crate::Ident;
use crate::Path;
use crate::Type;
use crate::TypeNode;

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
    ///A set of bound Lifetimes: `for<'a, 'b, 'c>`.
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
    pub(crate) path: Path,
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
        let generic_params = syn_to_generic_params(generics.params);
        let generic_params = if let Some(where_clause) = generics.where_clause {
            where_clause
                .predicates
                .into_iter()
                .map(|predicate| match predicate {
                    syn::WherePredicate::Type(syn::PredicateType {
                        lifetimes,
                        bounded_ty,
                        bounds,
                        ..
                    }) => GenericParam::Type(PredicateType {
                        lifetimes: syn_to_lifetimes(lifetimes),
                        bounded_ty: Type::syn_to_type(bounded_ty),
                        bounds: syn_to_type_param_bounds(bounds),
                    }),
                    syn::WherePredicate::Lifetime(lifetime) => unimplemented!(),
                    syn::WherePredicate::Eq(_eq) => unimplemented!("Generics::syn_to_generics: Eq"),
                })
                .collect()
        } else {
            generic_params.collect()
        };
        Generics {
            params: generic_params,
        }
    }
}

fn syn_to_lifetimes(lifetimes: Option<syn::BoundLifetimes>) -> Vec<Lifetime> {
    lifetimes.map_or_else(Vec::new, |lifetimes| {
        lifetimes
            .lifetimes
            .into_iter()
            .map(
                |syn::LifetimeDef {
                     lifetime: syn::Lifetime { ident, .. },
                     ..
                 }| Lifetime {
                    ident: Ident::from(ident),
                },
            )
            .collect()
    })
}

fn syn_to_generic_params<T>(params: T) -> impl Iterator<Item = GenericParam>
where
    T: IntoIterator<Item = syn::GenericParam>,
{
    params.into_iter().map(|param| match param {
        syn::GenericParam::Type(syn::TypeParam { ident, bounds, .. }) => {
            GenericParam::Type(PredicateType {
                lifetimes: Vec::new(),
                bounded_ty: Type(TypeNode::Path(Path::ident_to_path(Ident::from(ident)))),
                bounds: bounds
                    .into_iter()
                    .map(|bound| match bound {
                        syn::TypeParamBound::Lifetime(syn::Lifetime { ident, .. }) => {
                            TypeParamBound::Lifetime(Lifetime {
                                ident: Ident::from(ident),
                            })
                        }

                        syn::TypeParamBound::Trait(syn::TraitBound {
                            lifetimes, path, ..
                        }) => TypeParamBound::Trait(TraitBound {
                            lifetimes: syn_to_lifetimes(lifetimes),
                            path: Path::syn_to_path(path),
                        }),
                    })
                    .collect(),
            })
        }
        syn::GenericParam::Lifetime(syn::LifetimeDef {
            lifetime: syn::Lifetime { ident, .. },
            bounds,
            ..
        }) => GenericParam::Lifetime(LifetimeDef {
            ident: Ident::from(ident),
            bounds: bounds
                .into_iter()
                .map(|syn::Lifetime { ident, .. }| Lifetime {
                    ident: Ident::from(ident),
                })
                .collect(),
        }),
        syn::GenericParam::Const(_const) => unimplemented!("Generics::syn_to_generics: Const"),
    })
}

fn syn_to_type_param_bounds<T>(bounds: T) -> Vec<TypeParamBound>
where
    T: IntoIterator<Item = syn::TypeParamBound>,
{
    bounds
        .into_iter()
        .map(|type_param_bound| match type_param_bound {
            syn::TypeParamBound::Trait(syn::TraitBound {
                lifetimes, path, ..
            }) => TypeParamBound::Trait(TraitBound {
                lifetimes: syn_to_lifetimes(lifetimes),
                path: Path::syn_to_path(path),
            }),
            syn::TypeParamBound::Lifetime(lifetime) => TypeParamBound::Lifetime(Lifetime {
                ident: Ident::from(lifetime.ident),
            }),
        })
        .collect()
}
