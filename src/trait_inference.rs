use crate::{
    AngleBracketedGenericArguments, CompleteFunction, CompleteImpl, GenericArgument,
    GenericArguments, GenericConstraint, GenericParam, GlobalBorrow, Lifetime, Parent, ParentKind,
    Path, PathArguments, PredicateType, Push, Receiver, TraitBound, Type, TypeEqualitySetRef,
    TypeNode, TypeParamBound, INVOKES,
};
// SeaHasher is used, both because it is a faster hashing algorithm than the
// default one, and because it has a hasher with a defalt seed, which is
// useful for testing purposes, and consistent output between compiles.
use seahash::SeaHasher;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::BuildHasherDefault;
use std::iter::Extend;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

/// A set of types that are considered to be equal. An example of how it is used:
/// Say we have a function: fn func<T>(maybe: Option<T>) {}, and we call this
/// function with a value of type ::std::option::Option<String>. Then we need
/// at least two sets.
/// In set one, we have { Option<T>, ::std::option::Option<String>, .. }
/// In set two, we have { T, String, .. }. Both sets may contain more than two
/// types, since more than two types may be considered equal
pub(crate) struct TypeEqualitySet {
    pub(crate) set: HashSet<Type, BuildHasherDefault<SeaHasher>>,
}

/// A set of constraints used in the where clause in the final impl
pub(crate) struct ConstraintSet {
    pub(crate) set: HashSet<GenericConstraint, BuildHasherDefault<SeaHasher>>,
}

// A mapping between types and it's corresponding set of equal types
pub(crate) struct TypeEqualitySets {
    pub(crate) set_map: HashMap<Type, TypeEqualitySetRef, BuildHasherDefault<SeaHasher>>,
    pub(crate) sets: Vec<TypeEqualitySet>,
}

pub(crate) struct TraitInferenceResult {
    pub(crate) constraints: ConstraintSet,
    pub(crate) generic_params: BTreeSet<GenericParam>,
    pub(crate) data_struct_args: GenericArguments,
    pub(crate) trait_args: GenericArguments,
}

/// A mapping between a lifetime and it's subtypes
pub(crate) struct LifetimeSubtypeMap {
    subtypes: BTreeSet<(Lifetime, Lifetime)>,
}

/// N x N bool matrix
pub(crate) struct BoolMatrix {
    size: usize,
    matrix: Vec<bool>,
}

impl BoolMatrix {
    fn new(size: usize) -> Self {
        BoolMatrix {
            size,
            matrix: vec![false; size * size],
        }
    }
}

impl Index<(usize, usize)> for BoolMatrix {
    type Output = bool;
    fn index(&self, (row, column): (usize, usize)) -> &Self::Output {
        &self.matrix[self.size * row + column]
    }
}

impl IndexMut<(usize, usize)> for BoolMatrix {
    fn index_mut(&mut self, (row, column): (usize, usize)) -> &mut Self::Output {
        &mut self.matrix[self.size * row + column]
    }
}

impl LifetimeSubtypeMap {
    fn new() -> Self {
        LifetimeSubtypeMap {
            subtypes: BTreeSet::new(),
        }
    }

    fn insert(&mut self, subtype: Lifetime, supertype: Lifetime) {
        self.subtypes.insert((subtype, supertype));
    }

    fn transitive_closure(&mut self) {
        let (mapping, index_lifetime_mapping) = self.create_mapping();
        let size = mapping.len();
        let mut subtype_matrix = BoolMatrix::new(size);
        let mut reached = BoolMatrix::new(size);

        // Setting initial states
        for idx in mapping {
            subtype_matrix[idx] = true;
        }

        for i in 0..subtype_matrix.size {
            // Every type is a subtype of itself
            Self::transitive_closure_dfs(&mut subtype_matrix, &mut reached, i, i);
        }

        // Update self with transitive closure
        for subtype_index in 0..reached.size {
            let subtype = if let Some(&subtype) = index_lifetime_mapping.get(&subtype_index) {
                subtype
            } else {
                continue;
            };
            for supertype_index in 0..reached.size {
                let supertype =
                    if let Some(&supertype) = index_lifetime_mapping.get(&supertype_index) {
                        supertype
                    } else {
                        continue;
                    };
                if reached[(subtype_index, supertype_index)] {
                    self.subtypes.insert((subtype, supertype));
                }
            }
        }
    }

    fn transitive_closure_dfs(
        subtype_matrix: &mut BoolMatrix,
        reached: &mut BoolMatrix,
        subtype: usize,
        supertype: usize,
    ) {
        reached[(subtype, supertype)] = true;
        for i in subtype..subtype_matrix.size {
            // if i is a supertype of supertype then i is a supertype of subtype
            if subtype_matrix[(supertype, i)] && !reached[(supertype, i)] {
                Self::transitive_closure_dfs(subtype_matrix, reached, subtype, i);
            }
        }
    }

    /// Map all lifetimes to a unique index starting at zero
    /// The static lifetime always gets mapped to 0
    fn create_mapping(&self) -> (Vec<(usize, usize)>, BTreeMap<usize, Lifetime>) {
        let mut lifetime_index_mapping = BTreeMap::new();
        let mut index_lifetime_mapping = BTreeMap::new();
        let mut mapping = Vec::new();
        let mut counter: usize = 1;

        // Map the static lifetime to 0
        lifetime_index_mapping.insert(Lifetime(0), 0);
        for (subtype, supertype) in self.subtypes.iter() {
            let subtype = if let Some(&subtype) = lifetime_index_mapping.get(subtype) {
                subtype
            } else {
                lifetime_index_mapping.insert(*subtype, counter);
                let subtype = counter;
                counter += 1;
                subtype
            };
            let supertype = if let Some(&supertype) = lifetime_index_mapping.get(supertype) {
                supertype
            } else {
                lifetime_index_mapping.insert(*supertype, counter);
                let supertype = counter;
                counter += 1;
                supertype
            };
            mapping.push((subtype, supertype));
        }
        for (lifetime, index) in lifetime_index_mapping {
            index_lifetime_mapping.insert(index, lifetime);
        }
        (mapping, index_lifetime_mapping)
    }
}

impl ConstraintSet {
    fn new() -> Self {
        ConstraintSet {
            set: HashSet::default(),
        }
    }

    fn insert(&mut self, constraint: GenericConstraint) -> bool {
        self.set.insert(constraint)
    }

    fn contains(&self, constraint: &GenericConstraint) -> bool {
        self.set.contains(constraint)
    }
}

impl TypeEqualitySet {
    fn new() -> Self {
        TypeEqualitySet {
            set: HashSet::default(),
        }
    }

    fn contains(&self, ty: &Type) -> bool {
        self.set.contains(ty)
    }

    fn insert(&mut self, ty: Type) -> bool {
        self.set.insert(ty)
    }
}

impl TypeEqualitySetRef {
    /// The most concrete type is what the inferred type for a value must be.
    /// What is meant by making something more concrete, is essentially making
    /// it less generic. Say we have a TypeEqualitySet with these types:
    /// { T, Option<U> }. The most concrete type of these, are Option<U>.
    /// Imaginge then that we have another set: { U, String }. String is more
    /// concrete than U, and thus Option<String> is more concrete than Option<U>,
    /// and thus the most concrete type we can get startng from the first set
    /// is Option<String>
    fn make_most_concrete(
        &self,
        most_concrete_type_map: &mut BTreeMap<Self, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> TypeNode {
        let most_concrete = most_concrete_type_map.get(self);
        match most_concrete {
            Some(node) => node.clone(),
            None => {
                // Adding the Infer type as a temporary value is done for safety in case
                // of self referential type constraints. Say we have deduced that
                // Vec<&str> must be equal to &str, due to calling a function with the
                // wrong type. We then run the risk calling this method in an infinite
                // loop, since make_most_concrete_pair will call this method again with
                // the set containing &str, since it is the inner type of Vec<&str>, but
                // that is the same set as the current one, so we need a way to break the
                // loop. Since this method always checks the most_concrete_type_map first,
                // it will just return Infer, in case we have a self referential loop.
                most_concrete_type_map.insert(*self, TypeNode::Infer);

                let set = &type_equality_sets.sets[self.0].set;
                let mut iterator = set.iter().peekable();
                let first = iterator.next().unwrap().clone().0;
                let most_concrete = if iterator.peek().is_none() {
                    first.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
                } else {
                    iterator.fold(first, |current_most_concrete, ty| {
                        TypeNode::make_most_concrete_from_pair(
                            current_most_concrete,
                            ty.clone().0,
                            most_concrete_type_map,
                            type_equality_sets,
                        )
                    })
                };
                most_concrete_type_map.insert(*self, most_concrete.clone());
                most_concrete
            }
        }
    }
}

impl TypeEqualitySets {
    fn new() -> Self {
        TypeEqualitySets {
            set_map: HashMap::default(),
            sets: Vec::new(),
        }
    }

    fn contains_key(&self, ty: &Type) -> bool {
        self.set_map.contains_key(ty)
    }

    fn get_set(&self, ty: &Type) -> Option<&TypeEqualitySet> {
        self.set_map.get(ty).map(|set_ref| &self.sets[set_ref.0])
    }

    pub(crate) fn get_set_ref(&self, ty: &Type) -> Option<TypeEqualitySetRef> {
        self.set_map.get(ty).copied()
    }

    fn new_set(&mut self, ty: Type) -> TypeEqualitySetRef {
        let mut set = TypeEqualitySet::new();
        set.insert(ty.clone());
        let set_ref = self.sets.index_push(set);

        self.set_map.insert(ty, set_ref);
        set_ref
    }

    /// Insert two types as equal to eachother, and in case of TraitObjects, e.g.
    /// ty1: T, ty2: dyn Clone, insert T: Clone as constraint.
    fn insert_as_equal_to(&mut self, ty1: Type, ty2: Type, constraints: &mut ConstraintSet) {
        use TypeNode::*;
        match (&ty1.0, &ty2.0) {
            (TraitObject(bounds1), TraitObject(bounds2)) => {
                if bounds1.len() != bounds2.len() {
                    panic!("TypeEqualitySets::insert_as_equal_to: TraitObjects have different number of bounds")
                }
                return self.insert_inner_type_as_equal_to(&ty1, &ty2, constraints);
            }
            (TraitObject(bounds), _) => {
                constraints.insert(GenericConstraint::Type(PredicateType {
                    lifetimes: Vec::new(),
                    bounded_ty: ty2,
                    bounds: bounds.clone(),
                }));
                return;
            }
            (_, TraitObject(bounds)) => {
                constraints.insert(GenericConstraint::Type(PredicateType {
                    lifetimes: Vec::new(),
                    bounded_ty: ty1.clone(),
                    bounds: bounds.clone(),
                }));
                return;
            }
            // A reference and a mutable reference are not equal, but a mutable reference may conform to a
            // normal reference, so the inner types may be considered equal
            (Reference { inner: inner1, .. }, ReferenceMut { inner: inner2, .. }) => {
                return self.insert_as_equal_to(
                    Type(*inner1.clone()),
                    Type(*inner2.clone()),
                    constraints,
                )
            }
            (ReferenceMut { inner: inner1, .. }, Reference { inner: inner2, .. }) => {
                return self.insert_as_equal_to(
                    Type(*inner1.clone()),
                    Type(*inner2.clone()),
                    constraints,
                )
            }
            _ => (),
        }
        self.insert_inner_type_as_equal_to(&ty1, &ty2, constraints);
        match (self.set_map.get(&ty1), self.set_map.get(&ty2)) {
            (Some(&set_ref1), Some(&set_ref2)) => {
                let set2 = std::mem::replace(&mut self.sets[set_ref2.0], TypeEqualitySet::new());
                let set1 = &mut self.sets[set_ref1.0];
                for ty in set2.set.iter() {
                    if let Some(set_ref) = self.set_map.get_mut(ty) {
                        *set_ref = set_ref1
                    }
                }
                set1.set.extend(set2.set);
            }
            (Some(&set_ref), None) => {
                self.sets[set_ref.0].insert(ty2.clone());
                self.set_map.insert(ty2, set_ref);
            }
            (None, Some(&set_ref)) => {
                self.sets[set_ref.0].insert(ty1.clone());
                self.set_map.insert(ty1, set_ref);
            }
            (None, None) => {
                let mut set = TypeEqualitySet::new();
                set.insert(ty2.clone());
                set.insert(ty1.clone());
                let set_ref = self.sets.index_push(set);
                self.set_map.insert(ty2, set_ref);
                self.set_map.insert(ty1, set_ref);
            }
        }
    }

    /// Insert the inner types of two types as equal to eachother
    /// For example if we have two tuple types (T, &str) and (U, S) we would
    /// get two sets {T, U}, and {&str, S}
    fn insert_inner_type_as_equal_to(
        &mut self,
        ty1: &Type,
        ty2: &Type,
        constraints: &mut ConstraintSet,
    ) {
        use TypeNode::*;
        match (&ty1.0, &ty2.0) {
            (Tuple(types1), Tuple(types2)) => {
                if types1.len() == types2.len() {
                    types1.iter().zip(types2.iter()).for_each(|(ty1, t2)| {
                        self.insert_as_equal_to(ty1.clone(), t2.clone(), constraints)
                    })
                } else {
                    panic!("TypeEqualitySets::insert_inner_type_as_equal_to: Tuples have different number of arguments")
                }
            }
            (Reference { inner: inner1, .. }, Reference { inner: inner2, .. }) => {
                self.insert_as_equal_to(Type(*inner1.clone()), Type(*inner2.clone()), constraints)
            }
            (ReferenceMut { inner: inner1, .. }, ReferenceMut { inner: inner2, .. }) => {
                self.insert_as_equal_to(Type(*inner1.clone()), Type(*inner2.clone()), constraints)
            }
            (Path(path1), Path(path2)) => {
                self.insert_path_arguments_as_equal_to(path1, path2, constraints);
            }
            (TraitObject(bounds1), TraitObject(bounds2)) => {
                bounds1.iter().zip(bounds2.iter()).for_each(
                    |bounds| {
                        if let (
                            TypeParamBound::Trait(trait_bound1),
                            TypeParamBound::Trait(trait_bound2),
                        ) = bounds
                        {
                            self.insert_path_arguments_as_equal_to(
                                &trait_bound1.path,
                                &trait_bound2.path,
                                constraints,
                            );
                        }
                    }, //FIXME properly deal with lifetimes
                )
            }
            _ => (),
        }
    }

    /// When comparing two paths with the same number of arguments, we assume
    /// those arguments to correspond to eachother in the order they are
    /// defined. If we have two paths: ::std::result::Result<T, U>, and
    /// Result<V, W>, we assume T = V and U = W. This may not be the case
    /// in all cases as Result could be defined as:
    /// type Result<V, W> = std::result:Result<W, V>, but it is unlikely that
    /// that someone would define a type like that. The benifit of pretending
    /// that this scenario will not occur, is that we may get better trait
    /// inference for the cases where the type parameters correspond to
    /// eachother.
    ///
    /// For cases where two paths have an unequal number of parameters, we
    /// assume that the path with the fewest paramters is a type alias for
    /// the type with more parameters. Unfortunately, it is not possible to
    /// know which paramter corresponds to which, and thus the parameters can
    /// not be compared.
    fn insert_path_arguments_as_equal_to(
        &mut self,
        path1: &Path,
        path2: &Path,
        constraints: &mut ConstraintSet,
    ) {
        let (segment1, segment2) = (
            &path1.path[path1.path.len() - 1],
            &path2.path[path2.path.len() - 1],
        );
        match (&segment1.args, &segment2.args) {
            (PathArguments::AngleBracketed(args1), PathArguments::AngleBracketed(args2))
                if args1.args.args.len() == args2.args.args.len() =>
            {
                args1
                    .args
                    .args
                    .iter()
                    .zip(args2.args.args.iter())
                    .for_each(|args| match args {
                        (GenericArgument::Type(ty1), GenericArgument::Type(ty2)) => {
                            self.insert_as_equal_to(ty1.clone(), ty2.clone(), constraints)
                        }
                        _ => {
                            unimplemented!("TypeEqualitySets::insert_inner_type_as_equal_to: Path")
                        }
                    })
            }
            (PathArguments::Parenthesized(args1), _) => unimplemented!(
                "TypeEqualitySets::insert_inner_type_as_equal_to: ParenthesizedGenericArgument"
            ),
            (_, PathArguments::Parenthesized(args1)) => unimplemented!(
                "TypeEqualitySets::insert_inner_type_as_equal_to: ParenthesizedGenericArgument"
            ),
            _ => (),
        }
    }
}

impl CompleteImpl {
    pub(crate) fn compute_trait_bounds(&self) -> TraitInferenceResult {
        let mut constraints = ConstraintSet::new();
        let mut type_equality_sets = TypeEqualitySets::new();
        let mut original_generic_params = Vec::new();
        let mut original_data_struct_args = Vec::new();
        let mut original_trait_args = Vec::new();
        let mut most_concrete_type_map = BTreeMap::new();

        // data structure generics
        if let Type(TypeNode::DataStructure { ref generics, .. }) = self.ty {
            generics.constraints.iter().for_each(|constraint| {
                constraints.insert(constraint.clone());
            });
            generics.params.iter().for_each(|&param| {
                original_generic_params.push(param);
                original_data_struct_args.push(param);
            })
        };

        // trait generics
        if let Some((generics, path)) = self.trait_ty.as_ref().and_then(|trait_ty| {
            let generics = &trait_ty.generics;
            if generics.params.is_empty() {
                None
            } else {
                Some((generics, &trait_ty.path))
            }
        }) {
            generics.constraints.iter().for_each(|constraint| {
                constraints.insert(constraint.clone());
            });
            generics.params.iter().for_each(|&param| {
                original_generic_params.push(param);
            });
            match &path.path.last().unwrap().args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    original_trait_args.extend(args.args.args.iter().cloned());
                }
                _ => unreachable!(),
            }
        };

        self.functions.iter().for_each(|function| {
            function.compute_trait_bounds(&mut constraints, &mut type_equality_sets)
        });

        let relevant_generic_params = get_relevant_generic_params(
            &original_generic_params,
            &mut most_concrete_type_map,
            &mut type_equality_sets,
        );

        let constraints = constraints
            .set
            .into_iter()
            .filter_map(|constraint| {
                constraint.make_relevant(
                    &mut most_concrete_type_map,
                    &type_equality_sets,
                    &relevant_generic_params,
                )
            })
            .collect();

        let constraints = ConstraintSet { set: constraints };

        let data_struct_args = get_data_struct_args(
            original_data_struct_args,
            &mut most_concrete_type_map,
            &type_equality_sets,
        );

        let trait_args = get_trait_args(
            original_trait_args,
            &mut most_concrete_type_map,
            &type_equality_sets,
        );

        TraitInferenceResult {
            constraints,
            generic_params: relevant_generic_params,
            data_struct_args,
            trait_args,
        }
    }
}

impl CompleteFunction {
    fn compute_trait_bounds(
        &self,
        constraints: &mut ConstraintSet,
        type_equality_sets: &mut TypeEqualitySets,
    ) {
        use Receiver::*;
        INVOKES.with_borrow(|invokes| {
            for invoke in invokes[self.invokes.start.0..self.invokes.end.0].iter() {
                let parent = &invoke.function.parent;
                let sig = &invoke.function.sig;
                let args_iter = match sig.receiver {
                    NoSelf => {
                        assert_eq!(invoke.args.len(), sig.inputs.len());
                        invoke.args.iter()
                    }
                    reciever => {
                        assert_eq!(invoke.args.len(), sig.inputs.len() + 1);
                        let mut args_iter = invoke.args.iter();
                        let parent = parent.as_ref().unwrap();
                        let first_type = args_iter.next().unwrap().node().get_type();

                        match reciever {
                            SelfByValue => match parent.parent_kind {
                                ParentKind::Trait => {
                                    if let TypeNode::TypeParam(_) = &first_type.0 {
                                        add_self_trait_bound(parent, first_type, constraints)
                                    }
                                }
                                ParentKind::DataStructure => type_equality_sets.insert_as_equal_to(
                                    Type(TypeNode::Path(parent.path.clone())),
                                    first_type,
                                    constraints,
                                ),
                            },
                            SelfByReference(lifetime) => match parent.parent_kind {
                                ParentKind::Trait => {
                                    let first_type = first_type.dereference();
                                    if let TypeNode::TypeParam(_) = &first_type.0 {
                                        add_self_trait_bound(parent, first_type, constraints)
                                    }
                                }
                                ParentKind::DataStructure => type_equality_sets.insert_as_equal_to(
                                    Type(TypeNode::Reference {
                                        inner: Box::new(TypeNode::Path(parent.path.clone())),
                                        lifetime: lifetime.0,
                                    }),
                                    first_type,
                                    constraints,
                                ),
                            },
                            SelfByReferenceMut(lifetime) => match parent.parent_kind {
                                ParentKind::Trait => {
                                    let first_type = first_type.dereference();
                                    if let TypeNode::TypeParam(_) = &first_type.0 {
                                        add_self_trait_bound(parent, first_type, constraints)
                                    }
                                }
                                ParentKind::DataStructure => type_equality_sets.insert_as_equal_to(
                                    Type(TypeNode::ReferenceMut {
                                        inner: Box::new(TypeNode::Path(parent.path.clone())),
                                        lifetime: lifetime.0,
                                    }),
                                    first_type,
                                    constraints,
                                ),
                            },
                            NoSelf => unreachable!(),
                        }
                        args_iter
                    }
                };

                sig.inputs.iter().zip(args_iter).for_each(|(ty, val)| {
                    type_equality_sets.insert_as_equal_to(
                        ty.clone(),
                        val.node().get_type(),
                        constraints,
                    )
                });

                // Add parent constraints
                // FIXME: Add constraints from parent type
                if let Some(generics) = parent.as_ref().map(|parent| &parent.generics) {
                    generics.constraints.iter().for_each(|constraint| {
                        if !constraints.contains(constraint) {
                            constraints.insert(constraint.clone());
                        };
                    })
                };

                // Add function constraints
                // FIXME: Add constraints from types in signature
                sig.generics.constraints.iter().for_each(|constraint| {
                    if !constraints.contains(constraint) {
                        constraints.insert(constraint.clone());
                    };
                })
            }
        });
    }
}

fn add_self_trait_bound(parent: &Rc<Parent>, first_type: Type, constraints: &mut ConstraintSet) {
    constraints.insert(GenericConstraint::Type(PredicateType {
        lifetimes: Vec::new(),
        bounded_ty: first_type,
        bounds: vec![TypeParamBound::Trait(TraitBound {
            lifetimes: Vec::new(),
            path: parent.path.clone(),
        })],
    }));
}

/// Find generic parameters of the most concrete types for the generic
/// parameters related to the DataStructure or trait that is being
/// implemented. Then return a set containing all the inner generic type
/// parameters. Say we are implementing a trait Trait for a struct S<T, U>
/// After doing some analysis we have concluded that the most concrete type
/// for T is Option<V> and U is already the most concrete type it can be. We
/// then return a set containing V, and U
fn get_relevant_generic_params(
    original_generic_params: &[GenericParam],
    most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
    type_equality_sets: &mut TypeEqualitySets,
) -> BTreeSet<GenericParam> {
    use TypeNode::*;
    let mut relevant_generic_params = BTreeSet::new();

    original_generic_params
        .iter()
        .for_each(|param| match *param {
            GenericParam::Type(type_param) => {
                let type_param = param.type_param().unwrap();
                let set_ref = type_equality_sets.get_set_ref(&Type(TypeParam(type_param)));
                let set_ref = set_ref
                    .unwrap_or_else(|| type_equality_sets.new_set(Type(TypeParam(type_param))));

                let node = set_ref.make_most_concrete(most_concrete_type_map, type_equality_sets);
                node.inner_params(type_equality_sets, &mut relevant_generic_params)
            }
            // FIXME: Properly handle lifetimes
            lifetime @ GenericParam::Lifetime(_) => {
                relevant_generic_params.insert(lifetime);
            }

            GenericParam::Const(_) => unimplemented!(),
        });

    relevant_generic_params
}

fn get_data_struct_args(
    original_data_struct_args: Vec<GenericParam>,
    most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
    type_equality_sets: &TypeEqualitySets,
) -> GenericArguments {
    GenericArguments {
        args: original_data_struct_args
            .into_iter()
            .map(|arg| match arg {
                GenericParam::Type(ty) => GenericArgument::Type(Type(
                    TypeNode::TypeParam(ty)
                        .make_most_concrete(most_concrete_type_map, type_equality_sets),
                )),
                GenericParam::Lifetime(lifetime) => GenericArgument::Lifetime(lifetime),
                GenericParam::Const(_) => unimplemented!(),
            })
            .collect(),
    }
}

fn get_trait_args(
    original_trait_args: Vec<GenericArgument>,
    most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
    type_equality_sets: &TypeEqualitySets,
) -> GenericArguments {
    GenericArguments {
        args: original_trait_args
            .into_iter()
            .map(|arg| match arg {
                GenericArgument::Type(ty) => GenericArgument::Type(Type(
                    ty.0.make_most_concrete(most_concrete_type_map, type_equality_sets),
                )),

                // FIXME: properly handle lifetimes
                lifetime @ GenericArgument::Lifetime(_) => lifetime,

                _ => unimplemented!(),
            })
            .collect(),
    }
}

impl GenericConstraint {
    /// This method tries to make a constraint that uses the correctly
    /// inferred types based on the analysis done so far. There is a chance
    /// that the constraint is not relevant at all, and thus it returns
    /// Option<Self> instead if just Self.
    ///
    /// Example of a not relevat constraint is: `T: Clone`, where T is
    /// inferred to be String. String is a concrete type, and therefore does
    /// not need an explisit bound.
    ///
    /// Less obvious is that we disallow cases like:
    /// `Type<T>: Trait` and `U: Trait<Type<T>>`
    /// This is to avoid the scenario where Type is private but Trait is
    /// public. In this case the final impl may compile without specifying the
    /// trait bound, but won't compile with it.
    ///
    /// There are some corner cases where this could be done safely. If we are
    /// doing an `impl<T> Trait for Struct<Type<T>>` for example. In this case
    /// it is safe to accept the constraint: `Type<T>: Trait` as this won't
    /// trigger the private type in public interface error. This scenario is
    /// currently ignored by this method, and will return a None in that case,
    /// but it might be supported in the future.
    fn make_relevant(
        self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
        relevant_generic_params: &BTreeSet<GenericParam>,
    ) -> Option<Self> {
        let most_concrete = self.make_most_concrete(most_concrete_type_map, type_equality_sets);
        if most_concrete.is_relevant(type_equality_sets, relevant_generic_params) {
            Some(most_concrete)
        } else {
            None
        }
    }

    fn is_relevant(
        &self,
        type_equality_sets: &TypeEqualitySets,
        relevant_generic_params: &BTreeSet<GenericParam>,
    ) -> bool {
        match self {
            GenericConstraint::Type(pred_ty) => {
                pred_ty.is_relevant_for_constraint(type_equality_sets, relevant_generic_params)
            }

            //FIXME: Properly handle lifetimes
            GenericConstraint::Lifetime(_) => true,
        }
    }

    fn make_most_concrete(
        self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        match self {
            GenericConstraint::Type(pred_ty) => GenericConstraint::Type(
                pred_ty.make_most_concrete(most_concrete_type_map, type_equality_sets),
            ),
            GenericConstraint::Lifetime(_) =>
            //FIXME: Properly handle lifetimes
            {
                self
            }
        }
    }
}

impl PredicateType {
    fn is_relevant_for_constraint(
        &self,
        type_equality_sets: &TypeEqualitySets,
        relevant_generic_params: &BTreeSet<GenericParam>,
    ) -> bool {
        self.bounded_ty
            .0
            .is_relevant_for_constraint(&type_equality_sets, &relevant_generic_params)
            && self.bounds.iter().all(|bound| {
                bound.is_relevant_for_constraint(&type_equality_sets, &relevant_generic_params)
            })
    }

    fn make_most_concrete(
        self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        let bounded_ty = self
            .bounded_ty
            .0
            .make_most_concrete(most_concrete_type_map, type_equality_sets);

        let bounds = self
            .bounds
            .into_iter()
            .map(|bound| bound.make_most_concrete_inner(most_concrete_type_map, type_equality_sets))
            .collect();

        PredicateType {
            bounded_ty: Type(bounded_ty),
            bounds,
            // FIXME: lifetimes
            lifetimes: self.lifetimes,
        }
    }
}

impl TypeNode {
    fn is_relevant_for_constraint(
        &self,
        type_equality_sets: &TypeEqualitySets,
        relevant_generic_params: &BTreeSet<GenericParam>,
    ) -> bool {
        use TypeNode::*;
        match self {
            TypeParam(type_param) => {
                relevant_generic_params.contains(&GenericParam::Type(*type_param))
            }
            Reference { lifetime, inner } => {
                inner.is_relevant_for_constraint(type_equality_sets, relevant_generic_params)
            }
            ReferenceMut { lifetime, inner } => {
                inner.is_relevant_for_constraint(type_equality_sets, relevant_generic_params)
            }
            _ => false,
        }
    }

    fn make_most_concrete(
        self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        let ty = Type(self);
        if let Some(set_ref) = type_equality_sets.get_set_ref(&ty) {
            set_ref.make_most_concrete(most_concrete_type_map, type_equality_sets)
        } else {
            ty.0
        }
    }

    /// Makes the most concrete TypeNode from two TypeNodes that are consdered
    /// to be equal. This is primarily decided based on the inner types of the
    /// nodes.
    fn make_most_concrete_from_pair(
        ty1: TypeNode,
        ty2: TypeNode,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        use TypeNode::*;
        match (ty1, ty2) {
            (Infer, node) => {
                node.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
            }
            (node, Infer) => {
                node.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
            }
            (PrimitiveStr, _) => PrimitiveStr,
            (_, PrimitiveStr) => PrimitiveStr,
            (Path(path1), Path(path2)) => crate::Path::make_most_concrete_from_pair(
                path1,
                path2,
                most_concrete_type_map,
                type_equality_sets,
            ),
            (Path(path), _) => {
                Path(path.make_most_concrete_inner(most_concrete_type_map, type_equality_sets))
            }
            (_, Path(path)) => {
                Path(path.make_most_concrete_inner(most_concrete_type_map, type_equality_sets))
            }
            (Tuple(types1), Tuple(types2)) if types1.len() == types2.len() => Tuple(
                types1
                    .into_iter()
                    .zip(types2.into_iter())
                    .map(|(ty1, ty2)| {
                        Type(TypeNode::make_most_concrete_from_pair(
                            ty1.0,
                            ty2.0,
                            most_concrete_type_map,
                            type_equality_sets,
                        ))
                    })
                    .collect(),
            ),
            (Reference { inner: inner1, .. }, Reference { inner: inner2, .. }) => Reference {
                inner: Box::new(TypeNode::make_most_concrete_from_pair(
                    *inner1,
                    *inner2,
                    most_concrete_type_map,
                    type_equality_sets,
                )),
                // FIXME: deal with lifetimes
                lifetime: None,
            },
            (ReferenceMut { inner: inner1, .. }, ReferenceMut { inner: inner2, .. }) => {
                ReferenceMut {
                    inner: Box::new(TypeNode::make_most_concrete_from_pair(
                        *inner1,
                        *inner2,
                        most_concrete_type_map,
                        type_equality_sets,
                    )),
                    // FIXME: deal with lifetimes
                    lifetime: None,
                }
            }
            (TraitObject(_), node) => {
                node.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
            }
            (node, TraitObject(_)) => {
                node.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
            }
            (TypeParam(ref1), TypeParam(ref2)) => TypeParam(ref1.min(ref2)),
            (TypeParam(_), node) => {
                node.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
            }
            (node, TypeParam(_)) => {
                node.make_most_concrete_inner(most_concrete_type_map, type_equality_sets)
            }
            (node1, node2) => panic!(
                "TypeNode: make_most_concrete_pair: incompatible types \n{:#?}\nand\n{:#?}",
                node1, node2
            ),
        }
    }

    fn make_most_concrete_inner(
        self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        use TypeNode::*;
        match self {
            Tuple(types) => Tuple(
                types
                    .into_iter()
                    .map(|ty| {
                        Type(ty.0.make_most_concrete(most_concrete_type_map, type_equality_sets))
                    })
                    .collect(),
            ),
            Reference { inner, lifetime } => Reference {
                inner: Box::new(
                    inner.make_most_concrete(most_concrete_type_map, type_equality_sets),
                ),
                lifetime,
            },
            ReferenceMut { inner, lifetime } => ReferenceMut {
                inner: Box::new(
                    inner.make_most_concrete(most_concrete_type_map, type_equality_sets),
                ),
                lifetime,
            },
            Path(path) => {
                Path(path.make_most_concrete_inner(most_concrete_type_map, type_equality_sets))
            }
            node => node,
        }
    }

    fn inner_params(
        &self,
        type_equality_sets: &mut TypeEqualitySets,
        relevant_generic_params: &mut BTreeSet<GenericParam>,
    ) {
        use TypeNode::*;
        match self {
            Tuple(types) => {
                for ty in types.iter() {
                    ty.0.inner_params(type_equality_sets, relevant_generic_params)
                }
            }
            Reference { inner, .. } => {
                inner.inner_params(type_equality_sets, relevant_generic_params)
            }
            ReferenceMut { inner, .. } => {
                inner.inner_params(type_equality_sets, relevant_generic_params)
            }
            Path(path) => {
                path.inner_params(type_equality_sets, relevant_generic_params);
            }
            TypeParam(type_param) => {
                relevant_generic_params.insert(GenericParam::Type(*type_param));
            }
            _ => {}
        }
    }
}

impl TypeParamBound {
    fn is_relevant_for_constraint(
        &self,
        type_equality_sets: &TypeEqualitySets,
        relevant_generic_params: &BTreeSet<GenericParam>,
    ) -> bool {
        match self {
            TypeParamBound::Trait(bound) => {
                //FIXME: Properly deal with lifetimes
                bound
                    .path
                    .is_relevant_for_constraint(type_equality_sets, relevant_generic_params)
            }

            TypeParamBound::Lifetime(_) => {
                // FIXME: properly deal with lifetimes
                true
            }
        }
    }

    fn make_most_concrete_inner(
        self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        match self {
            TypeParamBound::Trait(bound) => {
                TypeParamBound::Trait(TraitBound {
                    path: bound
                        .path
                        .make_most_concrete_inner(most_concrete_type_map, type_equality_sets),
                    // FIXME: properly deal with lifetimes
                    lifetimes: bound.lifetimes,
                })
            }

            // FIXME: properly deal with lifetimes
            bound @ TypeParamBound::Lifetime(_) => bound,
        }
    }
}

impl Path {
    fn is_relevant_for_constraint(
        &self,
        type_equality_sets: &TypeEqualitySets,
        relevant_generic_params: &BTreeSet<GenericParam>,
    ) -> bool {
        self.path.iter().all(|segment| match &segment.args {
            PathArguments::None => true,

            PathArguments::AngleBracketed(args) => args.args.args.iter().all(|arg| match arg {
                GenericArgument::Type(ty) => {
                    ty.0.is_relevant_for_constraint(type_equality_sets, relevant_generic_params)
                }

                GenericArgument::Lifetime(_) => true,

                _ => unimplemented!("is_relevant_for_constraint: PathArguments::AngleBracketed"),
            }),

            PathArguments::Parenthesized(_) => {
                unimplemented!("is_relevant_for_constraint: PathArguments::Parenthesized")
            }
        })
    }

    fn inner_params(
        &self,
        type_equality_sets: &mut TypeEqualitySets,
        relevant_generic_params: &mut BTreeSet<GenericParam>,
    ) {
        for segment in self.path.iter() {
            match &segment.args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    for arg in args.args.args.iter() {
                        match arg {
                            GenericArgument::Type(ty) => {
                                ty.0.inner_params(type_equality_sets, relevant_generic_params)
                            }
                            GenericArgument::Lifetime(lifetime) => {
                                relevant_generic_params.insert(GenericParam::Lifetime(*lifetime));
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
                PathArguments::Parenthesized(_) => {
                    unimplemented!("Path::inner_param_set_ref: Parenthesized")
                }
            }
        }
    }

    fn make_most_concrete_inner(
        mut self,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> Self {
        let path_len = self.path.len();
        let segment = &mut self.path[path_len - 1];
        let mut path_arguments = std::mem::replace(&mut segment.args, PathArguments::None);
        path_arguments = match path_arguments {
            PathArguments::None => PathArguments::None,
            PathArguments::AngleBracketed(args) => {
                let args = args
                    .args
                    .args
                    .into_iter()
                    .map(|arg| match arg {
                        GenericArgument::Type(ty) => GenericArgument::Type(Type(
                            ty.0.make_most_concrete(most_concrete_type_map, type_equality_sets),
                        )),
                        GenericArgument::Lifetime(lifetime) => {
                            // FIXME: Lifetime
                            unimplemented!("Path::make_most_concrete_inner: Lifetime")
                        }
                        _ => unimplemented!(),
                    })
                    .collect();

                PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args: GenericArguments { args },
                })
            }
            PathArguments::Parenthesized(_) => {
                unimplemented!("Path::make_most_concrete_inner: Parenthesized")
            }
        };
        std::mem::replace(&mut segment.args, path_arguments);
        self
    }

    /// Compares two paths, and makes the most concrete path based on the two.
    ///
    /// The two paths may be different in length and number of arguments, but
    /// still represent the same underlying type. This methods accounts for
    /// this, and will try to combine these paths in the best way possible.
    /// It is for example common to make a custum Result type with one
    /// parameter, that is just a type alias for std::result::Result. e.g.:
    /// `type Result<T> = std::result::Result<Error, T>`
    ///
    /// This method prefers the path with the least arguments, as it will most
    /// likely be the most concrete, but this may not be the case in some
    /// circumstanses.
    ///
    /// For two paths with an equal number of generic arguments, these
    /// arguments are compared to eachother in chronlogical order. For example
    /// with the types: Result<T, U> and std::result::Result<&'static str, V>
    /// it is assumed that T is equal to &'static str, and U is equal to V
    ///
    /// In the rare case we have to types like Result<T, U> and tluseR<W, V>
    /// this will not hold if tluseR<W, V> is defined as:
    /// `type tluseR<W, V> = Result<V, W>;`.
    /// The code generation may produce invalid code in this case, but I
    /// assume this will be rare, and not worth worrying about.
    fn make_most_concrete_from_pair(
        mut path1: Path,
        mut path2: Path,
        most_concrete_type_map: &mut BTreeMap<TypeEqualitySetRef, TypeNode>,
        type_equality_sets: &TypeEqualitySets,
    ) -> TypeNode {
        let path1_len = path1.path.len();
        let path2_len = path2.path.len();
        let last_index1 = path1_len - 1;
        let last_index2 = path2_len - 1;
        let segment1 = &mut path1.path[last_index1];
        let segment2 = &mut path2.path[last_index2];
        match (&mut segment1.args, &mut segment2.args) {
            // Assume the path with the fewest arguments is the most concrete
            // since it is likely a type alias of the type with more arguments
            (PathArguments::None, _) => TypeNode::Path(path1),
            (_, PathArguments::None) => TypeNode::Path(path2),
            (PathArguments::AngleBracketed(args1), PathArguments::AngleBracketed(args2)) => {
                let args1 = &mut args1.args.args;
                let args2 = &mut args2.args.args;
                match args1.len().cmp(&args2.len()) {
                    Ordering::Less => TypeNode::Path(path1)
                        .make_most_concrete(most_concrete_type_map, type_equality_sets),

                    Ordering::Greater => TypeNode::Path(path2)
                        .make_most_concrete(most_concrete_type_map, type_equality_sets),

                    Ordering::Equal => {
                        // Assume we are dealing with the same type path
                        let args = args1
                            .iter()
                            .zip(args2.iter())
                            .map(|arg_pair| match arg_pair {
                                (GenericArgument::Type(ty1), GenericArgument::Type(ty2)) => {
                                    GenericArgument::Type(Type(
                                        TypeNode::make_most_concrete_from_pair(
                                            ty1.clone().0,
                                            ty2.clone().0,
                                            most_concrete_type_map,
                                            type_equality_sets,
                                        ),
                                    ))
                                }
                                // FIXME: Deal with lifetimes
                                (
                                    GenericArgument::Lifetime(lifetime_ref1),
                                    GenericArgument::Lifetime(lifetime_ref2),
                                ) => {
                                    GenericArgument::Lifetime((*lifetime_ref1).min(*lifetime_ref2))
                                }
                                _ => unimplemented!(
                                    "Path::make_most_concrete_from_pair: GenericArgument"
                                ),
                            })
                            .collect();

                        if path1.global || path1_len < path2_len {
                            *args1 = args;
                            TypeNode::Path(path1)
                        } else {
                            *args2 = args;
                            TypeNode::Path(path2)
                        }
                    }
                }
            }
            (PathArguments::Parenthesized(args1), PathArguments::Parenthesized(args2)) => {
                unimplemented!("Path::make_most_concrete_from_pair: Parenthesized")
            }
            _ => panic!("Path::make_most_concrete_from_pair: incompatible types"),
        }
    }
}
