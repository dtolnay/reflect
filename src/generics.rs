use crate::{Ident, LifetimeRef, Path, Push, Type, TypeNode, TypeParamRef};
use proc_macro2::Span;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::default::Default;
use syn::{parse_str, BoundLifetimes, PredicateLifetime, WhereClause, WherePredicate};

thread_local! {
    pub(crate) static TYPE_PARAMS: RefCell<Vec<TypeParam>> = RefCell::new(Vec::new());
    pub(crate) static LIFETIMES: RefCell<Vec<Lifetime>> = {
        let mut lifetimes = Vec::new();
        lifetimes.push(Lifetime { ident: Ident::new("static") });
        RefCell::new(lifetimes)
    };
}

#[derive(Debug, Clone)]
pub struct Generics {
    /// Represents the generic params without bounds.
    /// The bounds are moved to constraints.
    pub(crate) params: Vec<GenericParam>,

    /// Essentially represents the where clause
    pub(crate) constraints: Vec<GenericConstraint>,
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum GenericParam {
    Type(TypeParamRef),
    Lifetime(LifetimeRef),
    // Not supported
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
    pub(crate) lifetimes: Vec<LifetimeRef>,
    pub(crate) bounded_ty: Type,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone)]
pub(crate) enum TypeParamBound {
    Trait(TraitBound),
    Lifetime(LifetimeRef),
}

#[derive(Debug, Clone)]
pub(crate) struct TraitBound {
    /// A set of bound Lifetimes: `for<'a, 'b, 'c>`.
    pub(crate) lifetimes: Vec<LifetimeRef>,
    pub(crate) path: Path,
}

#[derive(Debug, Clone)]
pub(crate) struct Lifetime {
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone)]
pub(crate) struct LifetimeDef {
    pub(crate) ident: Ident,
    pub(crate) bounds: Vec<LifetimeRef>,
}

#[derive(Debug, Copy, Clone)]
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
    Lifetime(LifetimeRef),
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

pub struct ParamMap {
    pub(crate) map: BTreeMap<syn::Ident, GenericParam>,
}

impl ParamMap {
    pub(crate) fn new() -> Self {
        let static_lifetime = syn::Ident::new("static", Span::call_site());
        let mut param_map = BTreeMap::new();
        param_map.insert(static_lifetime, GenericParam::Lifetime(LifetimeRef(0)));
        ParamMap { map: param_map }
    }

    pub(crate) fn insert(&mut self, key: syn::Ident, value: GenericParam) -> Option<GenericParam> {
        self.map.insert(key, value)
    }

    pub(crate) fn get(&self, key: &syn::Ident) -> Option<&GenericParam> {
        self.map.get(key)
    }

    pub fn append(&mut self, other: &mut ParamMap) {
        self.map.append(&mut other.map)
    }
}

impl From<Lifetime> for LifetimeRef {
    fn from(lifetime: Lifetime) -> LifetimeRef {
        LIFETIMES.with(|lifetimes| lifetimes.borrow_mut().index_push(lifetime))
    }
}

impl From<TypeParam> for TypeParamRef {
    fn from(param: TypeParam) -> TypeParamRef {
        TYPE_PARAMS.with(|params| params.borrow_mut().index_push(param))
    }
}

impl GenericParam {
    pub(crate) fn lifetime_ref(self) -> Option<LifetimeRef> {
        match self {
            Self::Lifetime(lifetime_ref) => Some(lifetime_ref),
            _ => None,
        }
    }

    pub(crate) fn type_param_ref(self) -> Option<TypeParamRef> {
        match self {
            Self::Type(type_param_ref) => Some(type_param_ref),
            _ => None,
        }
    }
}

impl Generics {
    pub(crate) fn get_param_map(&self) -> ParamMap {
        let mut param_map = ParamMap::new();
        self.params.iter().for_each(|&param| match param {
            GenericParam::Type(type_param_ref) => {
                TYPE_PARAMS.with(|params| {
                    param_map.insert(
                        params.borrow()[type_param_ref.0].ident.0.clone(),
                        GenericParam::Type(type_param_ref),
                    )
                });
            }
            GenericParam::Lifetime(lifetime_ref) => {
                LIFETIMES.with(|lifetimes| {
                    param_map.insert(
                        lifetimes.borrow()[lifetime_ref.0].ident.0.clone(),
                        GenericParam::Lifetime(lifetime_ref),
                    )
                });
            }
            _ => unimplemented!(),
        });
        param_map
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> ParamMap {
        let syn_params = params.iter().map(|param| parse_str(param).unwrap());
        let (params, constraints, param_map) = syn_to_generic_params(syn_params);
        self.params.extend(params);
        self.constraints.extend(constraints);
        param_map
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str], param_map: &mut ParamMap) {
        let syn_constraints = constraints
            .iter()
            .map(|constraint| parse_str(constraint).unwrap());
        let constraints = syn_where_predicates_to_generic_constraints(syn_constraints, param_map);
        self.constraints.extend(constraints);
    }

    pub(crate) fn syn_to_generics(generics: syn::Generics) -> (Self, ParamMap) {
        let (params, mut constraints, mut param_map) = syn_to_generic_params(generics.params);
        if let Some(where_clause) = generics.where_clause {
            constraints.extend(syn_where_clause_to_generic_constraints(
                where_clause,
                &mut param_map,
            ));
        };
        (
            Generics {
                params,
                constraints,
            },
            param_map,
        )
    }
}

impl Default for Generics {
    fn default() -> Self {
        Generics {
            params: Vec::new(),
            constraints: Vec::new(),
        }
    }
}

fn syn_to_bound_lifetimes(
    lifetimes: Option<BoundLifetimes>,
    param_map: &mut ParamMap,
) -> Vec<LifetimeRef> {
    lifetimes.map_or_else(Vec::new, |lifetimes| {
        lifetimes
            .lifetimes
            .into_iter()
            .map(
                |syn::LifetimeDef {
                     lifetime: syn::Lifetime { ident, .. },
                     ..
                 }| {
                    let lifetime_ref = Lifetime {
                        ident: Ident::from(ident.clone()),
                    }
                    .into();
                    param_map.insert(ident, GenericParam::Lifetime(lifetime_ref));
                    lifetime_ref
                },
            )
            .collect()
    })
}

fn syn_where_clause_to_generic_constraints<'a>(
    where_clause: WhereClause,
    param_map: &'a mut ParamMap,
) -> impl Iterator<Item = GenericConstraint> + 'a {
    syn_where_predicates_to_generic_constraints(where_clause.predicates.into_iter(), param_map)
}

pub(crate) fn syn_where_predicates_to_generic_constraints<'a, I>(
    where_predicates: I,
    param_map: &'a mut ParamMap,
) -> impl Iterator<Item = GenericConstraint> + 'a
where
    I: Iterator<Item = WherePredicate> + 'a,
{
    where_predicates.map(move |predicate| match predicate {
        WherePredicate::Type(syn::PredicateType {
            lifetimes,
            bounded_ty,
            bounds,
            ..
        }) => GenericConstraint::Type(PredicateType {
            lifetimes: syn_to_bound_lifetimes(lifetimes, param_map),
            bounded_ty: Type::syn_to_type(bounded_ty, param_map),
            bounds: syn_to_type_param_bounds(bounds, param_map).collect(),
        }),
        WherePredicate::Lifetime(PredicateLifetime {
            lifetime: syn::Lifetime { ident, .. },
            bounds,
            ..
        }) => GenericConstraint::Lifetime(LifetimeDef {
            ident: Ident::from(ident),
            bounds: bounds
                .into_iter()
                .map(|syn::Lifetime { ident, .. }| {
                    param_map
                        .get(&ident)
                        .and_then(|&param| GenericParam::lifetime_ref(param))
                        .expect("syn_where_predicates_to_generic_constraints: Not a lifetime ref")
                })
                .collect(),
        }),
        WherePredicate::Eq(_eq) => unimplemented!("Generics::syn_to_generics: Eq"),
    })
}

pub(crate) fn syn_to_generic_params<T>(
    params: T,
) -> (Vec<GenericParam>, Vec<GenericConstraint>, ParamMap)
where
    T: IntoIterator<Item = syn::GenericParam>,
{
    let mut param_map = ParamMap::new();
    let mut constraints = Vec::new();
    let params: Vec<_> = params.into_iter().collect();
    params
        .iter()
        .for_each(|param| param_mapping(param, &mut param_map));
    let params = params
        .into_iter()
        .map(|param| match param {
            syn::GenericParam::Type(syn::TypeParam { ident, bounds, .. }) => {
                let &param = param_map.get(&ident).unwrap();
                let ident = Ident::from(ident);
                if !bounds.is_empty() {
                    constraints.push(GenericConstraint::Type(PredicateType {
                        lifetimes: Vec::new(),
                        bounded_ty: Type(TypeNode::TypeParam(
                            param
                                .type_param_ref()
                                .expect("syn_to_generic_params: Not a type param ref"),
                        )),
                        bounds: syn_to_type_param_bounds(bounds, &mut param_map).collect(),
                    }));
                }
                param
            }
            syn::GenericParam::Lifetime(syn::LifetimeDef {
                lifetime: syn::Lifetime { ident, .. },
                bounds,
                ..
            }) => {
                let &param = param_map.get(&ident).unwrap();
                if !bounds.is_empty() {
                    constraints.push(GenericConstraint::Lifetime(LifetimeDef {
                        ident: Ident::from(ident),
                        bounds: bounds
                            .into_iter()
                            .map(|syn::Lifetime { ident, .. }| {
                                param_map
                                    .get(&ident)
                                    .and_then(|param| GenericParam::lifetime_ref(*param))
                                    .expect("syn_to_generic_params: Not a lifetime ref")
                            })
                            .collect(),
                    }));
                }
                param
            }
            syn::GenericParam::Const(_const) => unimplemented!("Generics::syn_to_generics: Const"),
        })
        .collect();
    (params, constraints, param_map)
}

pub(crate) fn param_mapping(param: &syn::GenericParam, param_map: &mut ParamMap) {
    match &param {
        syn::GenericParam::Type(syn::TypeParam { ident, .. }) => {
            let param = GenericParam::Type(
                TypeParam {
                    ident: Ident::from(ident.clone()),
                }
                .into(),
            );
            param_map.insert(ident.clone(), param);
        }
        syn::GenericParam::Lifetime(syn::LifetimeDef {
            lifetime: syn::Lifetime { ident, .. },
            ..
        }) => {
            let param = GenericParam::Lifetime(
                Lifetime {
                    ident: Ident::from(ident.clone()),
                }
                .into(),
            );
            param_map.insert(ident.clone(), param);
        }
        syn::GenericParam::Const(_const) => unimplemented!("Generics::param_mapping: Const"),
    }
}

pub(crate) fn syn_to_type_param_bounds<'a, T>(
    bounds: T,
    param_map: &'a mut ParamMap,
) -> impl Iterator<Item = TypeParamBound> + 'a
where
    T: IntoIterator<Item = syn::TypeParamBound> + 'a,
{
    bounds
        .into_iter()
        .map(move |type_param_bound| match type_param_bound {
            syn::TypeParamBound::Trait(syn::TraitBound {
                lifetimes, path, ..
            }) => TypeParamBound::Trait(TraitBound {
                lifetimes: syn_to_bound_lifetimes(lifetimes, param_map),
                path: Path::syn_to_path(path, param_map),
            }),
            syn::TypeParamBound::Lifetime(lifetime) => TypeParamBound::Lifetime(
                param_map
                    .get(&lifetime.ident)
                    .and_then(|&param| GenericParam::lifetime_ref(param))
                    .expect("syn_to_type_param_bounds: Not a lifetime ref"),
            ),
        })
}

impl GenericArgument {
    pub(crate) fn syn_to_generic_argument(
        arg: syn::GenericArgument,
        param_map: &mut ParamMap,
    ) -> Self {
        match arg {
            syn::GenericArgument::Type(ty) => {
                GenericArgument::Type(Type::syn_to_type(ty, param_map))
            }

            syn::GenericArgument::Lifetime(lifetime) => GenericArgument::Lifetime(
                param_map
                    .get(&lifetime.ident)
                    .and_then(|&param| GenericParam::lifetime_ref(param))
                    .expect("syn_to_generic_argument: Not a lifetime ref"),
            ),

            syn::GenericArgument::Binding(binding) => GenericArgument::Binding(Binding {
                ident: Ident::from(binding.ident),
                ty: Type::syn_to_type(binding.ty, param_map),
            }),

            syn::GenericArgument::Constraint(constraint) => {
                GenericArgument::Constraint(Constraint {
                    ident: Ident::from(constraint.ident),
                    bounds: syn_to_type_param_bounds(constraint.bounds, param_map).collect(),
                })
            }

            syn::GenericArgument::Const(_expr) => {
                unimplemented!("GenericArguments::syn_to_generic_arguments: Const")
            }
        }
    }
}
