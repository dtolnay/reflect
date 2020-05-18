use crate::{Ident, Path, Type, TypeNode};
use syn::{BoundLifetimes, PredicateLifetime, WhereClause, WherePredicate};

#[derive(Debug, Clone)]
pub struct Generics {
    /// Represents the generic params without bounds.
    /// The bounds are moved to constraints.
    pub(crate) params: Vec<GenericParam>,

    /// Essentially represents the where clause
    pub(crate) constraints: Vec<GenericConstraint>,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericParam {
    Type(TypeParam),
    Lifetime(Lifetime),
    Const(ConstParam),
}

#[derive(Debug, Clone)]
pub(crate) struct TypeParam {
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericConstraint {
    Type(PredicateType),
    Lifetime(LifetimeDef),
}

#[derive(Debug, Clone)]
pub(crate) struct PredicateType {
    /// A set of bound Lifetimes: `for<'a, 'b, 'c>`.
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
    /// A set of bound Lifetimes: `for<'a, 'b, 'c>`.
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
    Type(Type),
    Lifetime(Lifetime),
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
    pub(crate) fn syn_to_generics(generics: syn::Generics) -> Self {
        let (params, mut constraints) = syn_to_generic_params(generics.params);
        if let Some(where_clause) = generics.where_clause {
            constraints.extend(syn_to_generic_constraints(where_clause));
        };
        Generics {
            params,
            constraints,
        }
    }
}

fn syn_to_bound_lifetimes(lifetimes: Option<BoundLifetimes>) -> Vec<Lifetime> {
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

fn syn_to_generic_constraints(
    where_clause: WhereClause,
) -> impl Iterator<Item = GenericConstraint> {
    where_clause
        .predicates
        .into_iter()
        .map(|predicate| match predicate {
            WherePredicate::Type(syn::PredicateType {
                lifetimes,
                bounded_ty,
                bounds,
                ..
            }) => GenericConstraint::Type(PredicateType {
                lifetimes: syn_to_bound_lifetimes(lifetimes),
                bounded_ty: Type::syn_to_type(bounded_ty),
                bounds: syn_to_type_param_bounds(bounds),
            }),
            WherePredicate::Lifetime(PredicateLifetime {
                lifetime: syn::Lifetime { ident, .. },
                bounds,
                ..
            }) => GenericConstraint::Lifetime(LifetimeDef {
                ident: Ident::from(ident),
                bounds: bounds
                    .into_iter()
                    .map(|syn::Lifetime { ident, .. }| Lifetime {
                        ident: Ident::from(ident),
                    })
                    .collect(),
            }),
            WherePredicate::Eq(_eq) => unimplemented!("Generics::syn_to_generics: Eq"),
        })
}

fn syn_to_generic_params<T>(params: T) -> (Vec<GenericParam>, Vec<GenericConstraint>)
where
    T: IntoIterator<Item = syn::GenericParam>,
{
    let mut constraints = Vec::new();
    let params = params
        .into_iter()
        .map(|param| match param {
            syn::GenericParam::Type(syn::TypeParam { ident, bounds, .. }) => {
                let ident = Ident::from(ident);
                if !bounds.is_empty() {
                    constraints.push(GenericConstraint::Type(PredicateType {
                        lifetimes: Vec::new(),
                        bounded_ty: Type(TypeNode::Path(Path::ident_to_path(ident.clone()))),
                        bounds: syn_to_type_param_bounds(bounds),
                    }));
                }

                GenericParam::Type(TypeParam { ident })
            }
            syn::GenericParam::Lifetime(syn::LifetimeDef {
                lifetime: syn::Lifetime { ident, .. },
                bounds,
                ..
            }) => {
                let ident = Ident::from(ident);
                if !bounds.is_empty() {
                    constraints.push(GenericConstraint::Lifetime(LifetimeDef {
                        ident: ident.clone(),
                        bounds: bounds
                            .into_iter()
                            .map(|syn::Lifetime { ident, .. }| Lifetime {
                                ident: Ident::from(ident),
                            })
                            .collect(),
                    }));
                }
                GenericParam::Lifetime(Lifetime { ident })
            }
            syn::GenericParam::Const(_const) => unimplemented!("Generics::syn_to_generics: Const"),
        })
        .collect();
    (params, constraints)
}

pub(crate) fn syn_to_type_param_bounds<T>(bounds: T) -> Vec<TypeParamBound>
where
    T: IntoIterator<Item = syn::TypeParamBound>,
{
    bounds
        .into_iter()
        .map(|type_param_bound| match type_param_bound {
            syn::TypeParamBound::Trait(syn::TraitBound {
                lifetimes, path, ..
            }) => TypeParamBound::Trait(TraitBound {
                lifetimes: syn_to_bound_lifetimes(lifetimes),
                path: Path::syn_to_path(path),
            }),
            syn::TypeParamBound::Lifetime(lifetime) => TypeParamBound::Lifetime(Lifetime {
                ident: Ident::from(lifetime.ident),
            }),
        })
        .collect()
}

impl GenericArgument {
    pub(crate) fn syn_to_generic_argument(arg: syn::GenericArgument) -> Self {
        match arg {
            syn::GenericArgument::Type(ty) => GenericArgument::Type(Type::syn_to_type(ty)),

            syn::GenericArgument::Lifetime(lifetime) => GenericArgument::Lifetime(Lifetime {
                ident: Ident::from(lifetime.ident),
            }),

            syn::GenericArgument::Binding(binding) => GenericArgument::Binding(Binding {
                ident: Ident::from(binding.ident),
                ty: Type::syn_to_type(binding.ty),
            }),

            syn::GenericArgument::Constraint(constraint) => {
                GenericArgument::Constraint(Constraint {
                    ident: Ident::from(constraint.ident),
                    bounds: syn_to_type_param_bounds(constraint.bounds),
                })
            }

            syn::GenericArgument::Const(_expr) => {
                unimplemented!("GenericArguments::syn_to_generic_arguments: Const")
            }
        }
    }
}
