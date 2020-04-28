use crate::{GlobalCounter, Ident, Path, Type, TypeNode, LIFETIMES, STATIC_LIFETIME, TYPE_PARAMS};
use proc_macro2::Span;
use std::collections::BTreeMap;
use std::default::Default;
use syn::{parse_str, BoundLifetimes, PredicateLifetime, WhereClause, WherePredicate};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generics {
    /// Represents the generic params without bounds.
    /// The bounds are moved to constraints.
    pub(crate) params: Vec<GenericParam>,

    /// Essentially represents the where clause
    pub(crate) constraints: Vec<GenericConstraint>,

    // A mapping between the parameter identifiers and their GenericParam
    // representation
    pub(crate) param_map: ParamMap,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum GenericParam {
    Lifetime(Lifetime),
    Type(TypeParam),
    // Not supported
    Const(ConstParam),
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct TypeParam(pub usize);

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct Lifetime(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum GenericConstraint {
    Type(PredicateType),
    Lifetime(LifetimeDef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct PredicateType {
    /// A set of bound Lifetimes: `for<'a, 'b, 'c>`.
    pub(crate) lifetimes: Vec<Lifetime>,
    pub(crate) bounded_ty: Type,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TypeParamBound {
    Trait(TraitBound),
    Lifetime(Lifetime),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TraitBound {
    /// A set of bound Lifetimes: `for<'a, 'b, 'c>`.
    pub(crate) lifetimes: Vec<Lifetime>,
    pub(crate) path: Path,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct LifetimeDef {
    pub(crate) lifetime: Lifetime,
    pub(crate) bounds: Vec<Lifetime>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ConstParam {
    pub(crate) private: (),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericArguments {
    pub(crate) args: Vec<GenericArgument>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum GenericArgument {
    Type(Type),
    Lifetime(Lifetime),
    Binding(Binding),
    Constraint(Constraint),
    Const(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Binding {
    pub(crate) ident: Ident,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Constraint {
    pub(crate) ident: Ident,
    pub(crate) bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Expr {
    pub(crate) private: (),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamMap {
    // TODO: Perhaps change key type from syn:Ident to syn::GenericParam or GenericParam
    // to avoid name collisions with type params and generic params with the same name
    pub(crate) map: BTreeMap<syn::Ident, GenericParam>,
}

pub struct RefMap {
    pub(crate) map: BTreeMap<GenericParam, GenericParam>,
}

impl ParamMap {
    pub(crate) fn new() -> Self {
        let static_lifetime = syn::Ident::new("static", Span::call_site());
        let mut param_map = BTreeMap::new();
        param_map.insert(static_lifetime, GenericParam::Lifetime(STATIC_LIFETIME));
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

    pub(crate) fn clone_with_fresh_generics(&self, ref_map: &RefMap) -> Self {
        let mut param_map = ParamMap::new();
        for key in self.map.keys() {
            let value = self.get(key).unwrap();

            if let Some(value) = ref_map.get(value) {
                param_map.insert(key.clone(), *value);
            }
        }
        param_map
    }
}

// TODO: Change name to something more descriptive
impl RefMap {
    pub(crate) fn new() -> Self {
        let mut map = BTreeMap::new();
        map.insert(
            GenericParam::Lifetime(STATIC_LIFETIME),
            GenericParam::Lifetime(STATIC_LIFETIME),
        );
        RefMap { map }
    }

    pub(crate) fn insert(
        &mut self,
        key: GenericParam,
        value: GenericParam,
    ) -> Option<GenericParam> {
        self.map.insert(key, value)
    }

    pub(crate) fn get(&self, key: &GenericParam) -> Option<&GenericParam> {
        self.map.get(key)
    }
}

impl TypeParamBound {
    pub(crate) fn get_type_param_bound(type_param_bound: &str, param_map: &mut ParamMap) -> Self {
        syn_to_type_param_bound(
            parse_str(type_param_bound)
                .expect("TypeParamBound::get_type_param_bound: Not a TypeParamBound"),
            param_map,
        )
    }

    pub(crate) fn clone_with_fresh_generics(&self, ref_map: &RefMap) -> Self {
        match self {
            TypeParamBound::Lifetime(lifetime) => {
                TypeParamBound::Lifetime(lifetime.clone_with_fresh_generics(ref_map))
            }

            TypeParamBound::Trait(bound) => TypeParamBound::Trait(TraitBound {
                lifetimes: bound
                    .lifetimes
                    .iter()
                    .map(|lifetime| lifetime.clone_with_fresh_generics(ref_map))
                    .collect(),

                path: bound.path.clone_with_fresh_generics(ref_map),
            }),
        }
    }
}

impl Lifetime {
    pub(crate) fn clone_with_fresh_generics(self, ref_map: &RefMap) -> Self {
        ref_map
            .get(&GenericParam::Lifetime(self))
            .and_then(|param| param.lifetime())
            .unwrap()
    }
}

impl GenericParam {
    pub(crate) fn lifetime(self) -> Option<Lifetime> {
        match self {
            Self::Lifetime(lifetime) => Some(lifetime),
            _ => None,
        }
    }

    pub(crate) fn type_param(self) -> Option<TypeParam> {
        match self {
            Self::Type(type_param) => Some(type_param),
            _ => None,
        }
    }

    pub(crate) fn get_fresh_param(self) -> Self {
        match self {
            Self::Type(type_param) => Self::Type(TYPE_PARAMS.count()),

            Self::Lifetime(lifetime) => Self::Lifetime(LIFETIMES.count()),

            Self::Const(_const) => unimplemented!("GenericParam::get_fresh_param: Const"),
        }
    }
}

impl GenericConstraint {
    pub(crate) fn clone_with_fresh_generics(&self, ref_map: &RefMap) -> Self {
        match self {
            Self::Type(predicate) => Self::Type(PredicateType {
                lifetimes: predicate
                    .lifetimes
                    .iter()
                    .map(|lifetime| lifetime.clone_with_fresh_generics(ref_map))
                    .collect(),
                bounded_ty: predicate.bounded_ty.clone_with_fresh_generics(ref_map),
                bounds: predicate
                    .bounds
                    .iter()
                    .map(|bound| bound.clone_with_fresh_generics(ref_map))
                    .collect(),
            }),
            Self::Lifetime(lifetime_def) => Self::Lifetime(LifetimeDef {
                lifetime: lifetime_def.lifetime.clone_with_fresh_generics(ref_map),
                bounds: lifetime_def
                    .bounds
                    .iter()
                    .map(|lifetime| lifetime.clone_with_fresh_generics(ref_map))
                    .collect(),
            }),
        }
    }
}

impl Generics {
    pub fn set_generic_params(&mut self, params: &[&str]) -> &mut ParamMap {
        let syn_params = params.iter().map(|param| parse_str(param).unwrap());
        let (params, constraints, mut param_map) = syn_to_generic_params(syn_params);
        self.params.extend(params);
        self.constraints.extend(constraints);
        self.param_map.append(&mut param_map);
        &mut self.param_map
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str]) {
        let syn_constraints = constraints
            .iter()
            .map(|constraint| parse_str(constraint).unwrap());
        let constraints =
            syn_where_predicates_to_generic_constraints(syn_constraints, &mut self.param_map);
        self.constraints.extend(constraints);
    }

    pub(crate) fn syn_to_generics(generics: syn::Generics) -> Self {
        let (params, mut constraints, mut param_map) = syn_to_generic_params(generics.params);
        if let Some(where_clause) = generics.where_clause {
            constraints.extend(syn_where_clause_to_generic_constraints(
                where_clause,
                &mut param_map,
            ));
        };
        Generics {
            params,
            constraints,
            param_map,
        }
    }

    pub(crate) fn clone_with_fresh_generics(&self) -> (Self, RefMap) {
        let mut ref_map = RefMap::new();
        (
            Generics {
                params: self
                    .params
                    .iter()
                    .map(|param| {
                        let new_param = param.get_fresh_param();
                        ref_map.insert(*param, new_param);
                        new_param
                    })
                    .collect(),
                constraints: self
                    .constraints
                    .iter()
                    .map(|constraint| constraint.clone_with_fresh_generics(&ref_map))
                    .collect(),
                param_map: self.param_map.clone_with_fresh_generics(&ref_map),
            },
            ref_map,
        )
    }
}

impl Default for Generics {
    fn default() -> Self {
        Generics {
            params: Vec::new(),
            constraints: Vec::new(),
            param_map: ParamMap::new(),
        }
    }
}

fn syn_to_bound_lifetimes(
    lifetimes: Option<BoundLifetimes>,
    param_map: &mut ParamMap,
) -> Vec<Lifetime> {
    lifetimes.map_or_else(Vec::new, |lifetimes| {
        lifetimes
            .lifetimes
            .into_iter()
            .map(
                |syn::LifetimeDef {
                     lifetime: syn::Lifetime { ident, .. },
                     ..
                 }| {
                    let lifetime = LIFETIMES.count();
                    param_map.insert(ident, GenericParam::Lifetime(lifetime));
                    lifetime
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
            lifetime: param_map
                .get(&ident)
                .and_then(|param| GenericParam::lifetime(*param))
                .expect("syn_where_predicates_to_generic_constraints: is not a lifetime ref"),
            bounds: bounds
                .into_iter()
                .map(|syn::Lifetime { ident, .. }| {
                    param_map
                        .get(&ident)
                        .and_then(|&param| GenericParam::lifetime(param))
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
                if !bounds.is_empty() {
                    constraints.push(GenericConstraint::Type(PredicateType {
                        lifetimes: Vec::new(),
                        bounded_ty: Type(TypeNode::TypeParam(
                            param
                                .type_param()
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
                        lifetime: param_map
                            .get(&ident)
                            .and_then(|param| GenericParam::lifetime(*param))
                            .expect("syn_to_generic_params: Not a lifetime ref"),
                        bounds: bounds
                            .into_iter()
                            .map(|syn::Lifetime { ident, .. }| {
                                param_map
                                    .get(&ident)
                                    .and_then(|param| GenericParam::lifetime(*param))
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
            let param = GenericParam::Type(TYPE_PARAMS.count());
            param_map.insert(ident.clone(), param);
        }
        syn::GenericParam::Lifetime(syn::LifetimeDef {
            lifetime: syn::Lifetime { ident, .. },
            ..
        }) => {
            let param = GenericParam::Lifetime(LIFETIMES.count());
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
        .map(move |type_param_bound| syn_to_type_param_bound(type_param_bound, param_map))
}

pub(crate) fn syn_to_type_param_bound(
    type_param_bound: syn::TypeParamBound,
    param_map: &mut ParamMap,
) -> TypeParamBound {
    match type_param_bound {
        syn::TypeParamBound::Trait(syn::TraitBound {
            lifetimes, path, ..
        }) => TypeParamBound::Trait(TraitBound {
            lifetimes: syn_to_bound_lifetimes(lifetimes, param_map),
            path: Path::syn_to_path(path, param_map),
        }),
        syn::TypeParamBound::Lifetime(lifetime) => TypeParamBound::Lifetime(
            param_map
                .get(&lifetime.ident)
                .and_then(|&param| GenericParam::lifetime(param))
                .expect("syn_to_type_param_bounds: Not a lifetime ref"),
        ),
    }
}

impl GenericArguments {
    pub(crate) fn clone_with_fresh_generics(&self, ref_map: &RefMap) -> Self {
        GenericArguments {
            args: self
                .args
                .iter()
                .map(|arg| arg.clone_with_fresh_generics(ref_map))
                .collect(),
        }
    }
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
                    .and_then(|&param| GenericParam::lifetime(param))
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

    pub(crate) fn clone_with_fresh_generics(&self, ref_map: &RefMap) -> Self {
        match self {
            Self::Type(ty) => Self::Type(ty.clone_with_fresh_generics(ref_map)),
            Self::Lifetime(lifetime) => Self::Lifetime(lifetime.clone_with_fresh_generics(ref_map)),
            Self::Binding(binding) => Self::Binding(Binding {
                ident: binding.ident.clone(),
                ty: binding.ty.clone_with_fresh_generics(ref_map),
            }),
            Self::Constraint(constraint) => Self::Constraint(Constraint {
                ident: constraint.ident.clone(),
                bounds: constraint
                    .bounds
                    .iter()
                    .map(|bound| bound.clone_with_fresh_generics(ref_map))
                    .collect(),
            }),
            Self::Const(expr) => {
                unimplemented!("GenericArgument::clone_with_fresh_generics: const expr")
            }
        }
    }
}
