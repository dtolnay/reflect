use crate::{
    CompleteFunction, CompleteImpl, GenericArgument, GenericArguments, GenericConstraint,
    GenericParam, GlobalBorrow, Invoke, Lifetime, LifetimeDef, Parent, ParentKind, Path,
    PathArguments, PredicateType, Push, Receiver, TraitBound, Type, TypeEqualitySetRef, TypeNode,
    TypeParamBound, TypedIndex, INVOKES, STATIC_LIFETIME, VALUES,
};
// SeaHasher is used, both because it is a faster hashing algorithm than the
// default one, and because it has a hasher with a defalt seed, which is
// useful for testing purposes, and consistent output between compiles.
use seahash::SeaHasher;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::convert::identity;
use std::hash::BuildHasherDefault;
use std::hash::Hash;
use std::iter::Extend;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

pub(crate) struct EqualitySet<T> {
    pub(crate) set: HashSet<T, BuildHasherDefault<SeaHasher>>,
}

/// A set of types that are considered to be equal. An example of how it is used:
/// Say we have a function: fn func<T>(maybe: Option<T>) {}, and we call this
/// function with a value of type ::std::option::Option<String>. Then we need
/// at least two sets.
/// In set one, we have { Option<T>, ::std::option::Option<String>, .. }
/// In set two, we have { T, String, .. }. Both sets may contain more than two
/// types, since more than two types may be considered equal
pub(crate) type TypeEqualitySet = EqualitySet<TypeNode>;

/// A set of constraints used in the where clause in the final impl
pub(crate) struct ConstraintSet {
    pub(crate) set: HashSet<GenericConstraint, BuildHasherDefault<SeaHasher>>,
}

pub(crate) struct EqualitySets<SetRef, T> {
    pub(crate) set_map: HashMap<T, SetRef, BuildHasherDefault<SeaHasher>>,
    pub(crate) sets: Vec<EqualitySet<T>>,
}

// A mapping between types and it's corresponding set of equal types
pub(crate) type TypeEqualitySets = EqualitySets<TypeEqualitySetRef, TypeNode>;

pub(crate) struct ConcreteMapAndSets {
    most_concrete_type_map: BTreeMap<TypeEqualitySetRef, TypeNode>,
    type_equality_sets: TypeEqualitySets,
}

pub(crate) struct OriginalGenercs {
    original_generic_params: Vec<GenericParam>,
    original_data_struct_args: Vec<GenericParam>,
    original_trait_args: Vec<GenericArgument>,
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

pub(crate) struct TransitiveClosure {
    transitive_closure: BoolMatrix,
    index_lifetime_mapping: BTreeMap<usize, Lifetime>,
    lifetime_index_mapping: BTreeMap<Lifetime, usize>,
}

struct Mapping {
    mapping: Vec<(usize, usize)>,
    index_lifetime_mapping: BTreeMap<usize, Lifetime>,
    lifetime_index_mapping: BTreeMap<Lifetime, usize>,
    size: usize,
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

    fn insert_as_equal(&mut self, subtype: Lifetime, supertype: Lifetime) {
        self.insert(subtype, supertype);
        self.insert(supertype, subtype);
    }

    fn add_lifetime_bounds(&mut self, constraints: &ConstraintSet) {
        constraints.set.iter().for_each(|constraint| {
            if let GenericConstraint::Lifetime(lifetime_def) = constraint {
                lifetime_def
                    .bounds
                    .iter()
                    .for_each(|&supertype| self.insert(lifetime_def.lifetime, supertype))
            }
        });
    }

    fn transitive_closure(self) -> TransitiveClosure {
        let Mapping {
            mapping,
            index_lifetime_mapping,
            lifetime_index_mapping,
            size,
        } = self.create_mapping();

        let mut subtype_graph = BoolMatrix::new(size);
        let mut transitive_closure = BoolMatrix::new(size);

        // Setting initial states
        for idx in mapping {
            subtype_graph[idx] = true;
        }

        for i in 0..subtype_graph.size {
            // The static lifetime is a subtype to all lifetimes
            subtype_graph[(0, i)] = true;
        }
        for i in 0..subtype_graph.size {
            // Every type is a subtype of itself
            Self::transitive_closure_dfs(&mut subtype_graph, &mut transitive_closure, i, i);
        }
        for i in 0..size {
            println!();
            for j in 0..size {
                print!("{} ", transitive_closure[(i, j)] as u8);
            }
        }

        TransitiveClosure {
            transitive_closure,
            lifetime_index_mapping,
            index_lifetime_mapping,
        }
    }

    fn transitive_closure_dfs(
        subtype_graph: &mut BoolMatrix,
        transitive_closure: &mut BoolMatrix,
        subtype: usize,
        supertype: usize,
    ) {
        transitive_closure[(subtype, supertype)] = true;
        for i in 0..subtype_graph.size {
            // if i is a supertype of supertype then i is a supertype of subtype
            if subtype_graph[(supertype, i)] && !transitive_closure[(subtype, i)] {
                Self::transitive_closure_dfs(subtype_graph, transitive_closure, subtype, i);
            }
        }
    }

    /// Map all lifetimes to a unique index starting at zero
    /// The static lifetime always gets mapped to 0
    fn create_mapping(&self) -> Mapping {
        let mut lifetime_index_mapping = BTreeMap::new();
        let mut index_lifetime_mapping = BTreeMap::new();
        let mut unique_lifetimes = BTreeSet::new();
        let mut mapping = Vec::new();

        unique_lifetimes.insert(STATIC_LIFETIME);
        for &(subtype, supertype) in &self.subtypes {
            unique_lifetimes.insert(subtype);
            unique_lifetimes.insert(supertype);
        }
        let size = unique_lifetimes.len();
        for (index, lifetime) in unique_lifetimes.into_iter().enumerate() {
            index_lifetime_mapping.insert(index, lifetime);
            lifetime_index_mapping.insert(lifetime, index);
        }
        for (subtype, supertype) in &self.subtypes {
            let &subtype = lifetime_index_mapping.get(subtype).unwrap();
            let &supertype = lifetime_index_mapping.get(supertype).unwrap();
            mapping.push((subtype, supertype));
        }

        Mapping {
            mapping,
            index_lifetime_mapping,
            lifetime_index_mapping,
            size,
        }
    }
}

impl TransitiveClosure {
    pub(crate) fn make_most_concrete_lifetime(&self, lifetime: &mut Lifetime) {
        if let Some(&lifetime_index) = self.lifetime_index_mapping.get(&lifetime) {
            // Find the first lifetime self is equal to
            *lifetime = (0..self.transitive_closure.size)
                .filter_map(|i| {
                    if self.transitive_closure[(i, lifetime_index)]
                        && self.transitive_closure[(lifetime_index, i)]
                    {
                        self.index_lifetime_mapping.get(&i).copied()
                    } else {
                        None
                    }
                })
                .min()
                .unwrap();
        };
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

    fn add_subtypes(&mut self, transitive_closure: &TransitiveClosure) {
        let lifetime_index_mapping = &transitive_closure.index_lifetime_mapping;
        let transitive_closure = &transitive_closure.transitive_closure;
        for subtype in 0..transitive_closure.size {
            for supertype in 0..transitive_closure.size {
                // Add the subtype: supertype constraint if subtype is a
                // subtype of supertype, and subtype is not equal supertype
                if transitive_closure[(subtype, supertype)]
                    && transitive_closure[(subtype, supertype)]
                        != transitive_closure[(supertype, subtype)]
                {
                    let subtype = *lifetime_index_mapping.get(&subtype).unwrap();
                    let supertype = *lifetime_index_mapping.get(&supertype).unwrap();
                    self.set.insert(GenericConstraint::Lifetime(LifetimeDef {
                        lifetime: subtype,
                        bounds: vec![supertype],
                    }));
                }
            }
        }
    }

    fn filter_constraints(
        self,
        relevant_generic_params: &BTreeSet<GenericParam>,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) -> Self {
        ConstraintSet {
            set: self
                .set
                .into_iter()
                .map(|mut constraint| {
                    let is_relevant = constraint.make_relevant(
                        concrete_maps_and_sets,
                        relevant_generic_params,
                        transitive_closure,
                    );
                    if is_relevant {
                        Some(constraint)
                    } else {
                        None
                    }
                })
                .filter_map(identity)
                .collect(),
        }
    }
}

impl<T> EqualitySet<T>
where
    T: Eq + Hash,
{
    fn new() -> Self {
        EqualitySet {
            set: HashSet::default(),
        }
    }

    fn contains(&self, t: &T) -> bool {
        self.set.contains(t)
    }

    fn insert(&mut self, t: T) -> bool {
        self.set.insert(t)
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
        self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) -> TypeNode {
        let most_concrete = concrete_maps_and_sets.most_concrete_type_map.get(&self);
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
                concrete_maps_and_sets
                    .most_concrete_type_map
                    .insert(self, TypeNode::Infer);

                // Temprorarily take the set out of the equlity set to avoid borrowing issues
                let set =
                    std::mem::take(&mut concrete_maps_and_sets.type_equality_sets.sets[self.0].set);
                let mut iterator = set.iter().peekable();
                let mut first = iterator.next().unwrap().clone();
                let most_concrete = if iterator.peek().is_none() {
                    first.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure);
                    first
                } else {
                    iterator.fold(first, |current_most_concrete, ty| {
                        TypeNode::make_most_concrete_from_pair(
                            current_most_concrete,
                            ty.clone(),
                            concrete_maps_and_sets,
                            transitive_closure,
                        )
                    })
                };
                concrete_maps_and_sets
                    .most_concrete_type_map
                    .insert(self, most_concrete.clone());

                // insert the set back in again
                concrete_maps_and_sets.type_equality_sets.sets[self.0].set = set;
                most_concrete
            }
        }
    }
}

impl<SetRef, T> EqualitySets<SetRef, T>
where
    SetRef: Copy,
    T: Eq + Hash + Clone,
    EqualitySet<T>: TypedIndex<Index = SetRef>,
{
    fn new() -> Self {
        EqualitySets {
            set_map: HashMap::default(),
            sets: Vec::new(),
        }
    }

    fn contains_key(&self, t: &T) -> bool {
        self.set_map.contains_key(t)
    }

    fn get_set(&self, t: &T) -> Option<&EqualitySet<T>> {
        self.set_map
            .get(t)
            .map(|set_ref| &self.sets[<EqualitySet<T> as TypedIndex>::from_index(*set_ref)])
    }

    pub(crate) fn get_set_ref(&self, t: &T) -> Option<SetRef> {
        self.set_map.get(t).copied()
    }

    fn new_set(&mut self, t: T) -> SetRef {
        let mut set = EqualitySet::new();
        set.insert(t.clone());
        let set_ref = self.sets.index_push(set);

        self.set_map.insert(t, set_ref);
        set_ref
    }

    fn insert_as_equal(&mut self, t1: T, t2: T) {
        match (self.set_map.get(&t1), self.set_map.get(&t2)) {
            (Some(&set_ref1), Some(&set_ref2)) => {
                let set2 = std::mem::replace(
                    &mut self.sets[<EqualitySet<T> as TypedIndex>::from_index(set_ref2)],
                    EqualitySet::new(),
                );
                let set1 = &mut self.sets[<EqualitySet<T> as TypedIndex>::from_index(set_ref1)];
                for t in &set2.set {
                    if let Some(set_ref) = self.set_map.get_mut(t) {
                        *set_ref = set_ref1
                    }
                }
                set1.set.extend(set2.set);
            }
            (Some(&set_ref), None) => {
                self.sets[<EqualitySet<T> as TypedIndex>::from_index(set_ref)].insert(t2.clone());
                self.set_map.insert(t2, set_ref);
            }
            (None, Some(&set_ref)) => {
                self.sets[<EqualitySet<T> as TypedIndex>::from_index(set_ref)].insert(t1.clone());
                self.set_map.insert(t1, set_ref);
            }
            (None, None) => {
                let mut set = EqualitySet::new();
                set.insert(t2.clone());
                set.insert(t1.clone());
                let set_ref = self.sets.index_push(set);
                self.set_map.insert(t2, set_ref);
                self.set_map.insert(t1, set_ref);
            }
        }
    }
}

impl TypeEqualitySets {
    /// Insert two types as equal to eachother, and in case of TraitObjects, e.g.
    /// ty1: T, ty2: dyn Clone, insert T: Clone as constraint.
    fn insert_types_as_equal(
        &mut self,
        ty1: TypeNode,
        ty2: TypeNode,
        constraints: &mut ConstraintSet,
        subtypes: &mut LifetimeSubtypeMap,
    ) {
        use TypeNode::*;
        match (&ty1, &ty2) {
            (TraitObject(bounds1), TraitObject(bounds2)) => {
                if bounds1.len() != bounds2.len() {
                    panic!("TypeEqualitySets::insert_types_as_equal: TraitObjects have different number of bounds")
                }
                return self.insert_inner_type_as_equal_to(&ty1, &ty2, constraints, subtypes);
            }
            (TraitObject(bounds), _) => {
                constraints.insert(GenericConstraint::Type(PredicateType {
                    lifetimes: Vec::new(),
                    bounded_ty: Type(ty2),
                    bounds: bounds.clone(),
                }));
                return;
            }
            (_, TraitObject(bounds)) => {
                constraints.insert(GenericConstraint::Type(PredicateType {
                    lifetimes: Vec::new(),
                    bounded_ty: Type(ty1.clone()),
                    bounds: bounds.clone(),
                }));
                return;
            }
            // A reference and a mutable reference are not equal, but a mutable reference may conform to a
            // normal reference, so the inner types may be considered equal
            (
                Reference {
                    is_mut: is_mut1,
                    lifetime: lifetime1,
                    inner: inner1,
                },
                Reference {
                    is_mut: is_mut2,
                    lifetime: lifetime2,
                    inner: inner2,
                },
            ) if is_mut1 != is_mut2 => {
                if let (Some(lifetime1), Some(lifetime2)) = (lifetime1, lifetime2) {
                    subtypes.insert_as_equal(*lifetime1, *lifetime2);
                }
                return self.insert_types_as_equal(
                    *inner1.clone(),
                    *inner2.clone(),
                    constraints,
                    subtypes,
                );
            }
            _ => (),
        }
        self.insert_inner_type_as_equal_to(&ty1, &ty2, constraints, subtypes);
        self.insert_as_equal(ty1, ty2)
    }

    /// Insert the inner types of two types as equal to eachother
    /// For example if we have two tuple types (T, &str) and (U, S) we would
    /// get two sets {T, U}, and {&str, S}
    fn insert_inner_type_as_equal_to(
        &mut self,
        ty1: &TypeNode,
        ty2: &TypeNode,
        constraints: &mut ConstraintSet,
        subtypes: &mut LifetimeSubtypeMap,
    ) {
        use TypeNode::*;
        match (ty1, ty2) {
            (Tuple(types1), Tuple(types2)) => {
                if types1.len() == types2.len() {
                    types1.iter().zip(types2.iter()).for_each(|(ty1, ty2)| {
                        self.insert_types_as_equal(
                            ty1.0.clone(),
                            ty2.0.clone(),
                            constraints,
                            subtypes,
                        )
                    })
                } else {
                    panic!("TypeEqualitySets::insert_inner_type_as_equal_to: Tuples have different number of arguments")
                }
            }
            (
                Reference {
                    is_mut: is_mut1,
                    lifetime: lifetime1,
                    inner: inner1,
                },
                Reference {
                    is_mut: is_mut2,
                    lifetime: lifetime2,
                    inner: inner2,
                },
            ) if is_mut1 == is_mut2 => {
                if let (Some(lifetime1), Some(lifetime2)) = (lifetime1, lifetime2) {
                    subtypes.insert_as_equal(*lifetime1, *lifetime2);
                }
                self.insert_types_as_equal(*inner1.clone(), *inner2.clone(), constraints, subtypes)
            }
            (Path(path1), Path(path2)) => {
                self.insert_path_arguments_as_equal_to(path1, path2, constraints, subtypes);
            }
            (TraitObject(bounds1), TraitObject(bounds2)) => bounds1
                .iter()
                .zip(bounds2.iter())
                .for_each(|bounds| match bounds {
                    (TypeParamBound::Trait(trait_bound1), TypeParamBound::Trait(trait_bound2)) => {
                        self.insert_path_arguments_as_equal_to(
                            &trait_bound1.path,
                            &trait_bound2.path,
                            constraints,
                            subtypes,
                        );
                    }
                    (TypeParamBound::Lifetime(lifetime1), TypeParamBound::Lifetime(lifetime2)) => {
                        subtypes.insert_as_equal(*lifetime1, *lifetime2);
                    }
                    _ => panic!("TraitObjects have different bound types"),
                }),
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
        subtypes: &mut LifetimeSubtypeMap,
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
                        (GenericArgument::Type(ty1), GenericArgument::Type(ty2)) => self
                            .insert_types_as_equal(
                                ty1.0.clone(),
                                ty2.0.clone(),
                                constraints,
                                subtypes,
                            ),
                        (
                            GenericArgument::Lifetime(lifetime1),
                            GenericArgument::Lifetime(lifetime2),
                        ) => {
                            subtypes.insert_as_equal(*lifetime1, *lifetime2);
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
        let mut subtypes = LifetimeSubtypeMap::new();

        let OriginalGenercs {
            original_generic_params,
            original_data_struct_args,
            original_trait_args,
        } = self.get_original_generics(&mut constraints);

        self.functions.iter().for_each(|function| {
            function.compute_trait_bounds(&mut constraints, &mut type_equality_sets, &mut subtypes)
        });

        subtypes.add_lifetime_bounds(&constraints);
        let transitive_closure = subtypes.transitive_closure();
        constraints.add_subtypes(&transitive_closure);

        let (mut relevant_generic_params, mut concrete_maps_and_sets) = get_relevant_generic_params(
            &original_generic_params,
            type_equality_sets,
            &transitive_closure,
        );

        let constraints = constraints.filter_constraints(
            &relevant_generic_params,
            &mut concrete_maps_and_sets,
            &transitive_closure,
        );

        let data_struct_args = get_data_struct_args(
            original_data_struct_args,
            &mut concrete_maps_and_sets,
            &transitive_closure,
        );

        let trait_args = get_trait_args(
            original_trait_args,
            &mut concrete_maps_and_sets,
            &transitive_closure,
        );

        // We remove the static lifetime since it is not a part of the paramater list
        relevant_generic_params.remove(&GenericParam::Lifetime(STATIC_LIFETIME));

        // FIXME: We need a mapping to the most concrete types and lifetimes in each function
        TraitInferenceResult {
            constraints,
            generic_params: relevant_generic_params,
            data_struct_args,
            trait_args,
        }
    }

    fn get_original_generics(&self, constraints: &mut ConstraintSet) -> OriginalGenercs {
        let mut original_generic_params = Vec::new();
        let mut original_data_struct_args = Vec::new();
        let mut original_trait_args = Vec::new();

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

        OriginalGenercs {
            original_generic_params,
            original_data_struct_args,
            original_trait_args,
        }
    }
}

impl CompleteFunction {
    fn compute_trait_bounds(
        &self,
        constraints: &mut ConstraintSet,
        type_equality_sets: &mut TypeEqualitySets,
        subtypes: &mut LifetimeSubtypeMap,
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
                                ParentKind::DataStructure => type_equality_sets
                                    .insert_types_as_equal(
                                        TypeNode::Path(parent.path.clone()),
                                        first_type.0,
                                        constraints,
                                        subtypes,
                                    ),
                            },
                            SelfByReference { is_mut, lifetime } => match parent.parent_kind {
                                ParentKind::Trait => {
                                    let first_type = first_type.dereference();
                                    if let TypeNode::TypeParam(_) = &first_type.0 {
                                        add_self_trait_bound(parent, first_type, constraints)
                                    }
                                }
                                ParentKind::DataStructure => type_equality_sets
                                    .insert_types_as_equal(
                                        TypeNode::Reference {
                                            is_mut,
                                            inner: Box::new(TypeNode::Path(parent.path.clone())),
                                            lifetime: lifetime.0,
                                        },
                                        first_type.0,
                                        constraints,
                                        subtypes,
                                    ),
                            },
                            NoSelf => unreachable!(),
                        }
                        args_iter
                    }
                };

                sig.inputs.iter().zip(args_iter).for_each(|(ty, val)| {
                    type_equality_sets.insert_types_as_equal(
                        ty.0.clone(),
                        val.node().get_type().0,
                        constraints,
                        subtypes,
                    )
                });

                Self::add_constraints(invoke, constraints, subtypes);
            }
        });

        self.set_output_equal_to_last_value(constraints, type_equality_sets, subtypes);
    }

    fn add_constraints(
        invoke: &Invoke,
        constraints: &mut ConstraintSet,
        subtypes: &mut LifetimeSubtypeMap,
    ) {
        // Add parent constraints
        // FIXME: Add constraints from parent type
        if let Some(generics) = invoke
            .function
            .parent
            .as_ref()
            .map(|parent| &parent.generics)
        {
            generics.constraints.iter().for_each(|constraint| {
                if !constraints.contains(constraint) {
                    constraints.insert(constraint.clone());
                };
                if let GenericConstraint::Lifetime(lifetime) = constraint {
                    for &supertype in &lifetime.bounds {
                        subtypes.insert(lifetime.lifetime, supertype);
                    }
                };
            })
        };

        // Add function constraints
        // FIXME: Add constraints from types in signature
        invoke
            .function
            .sig
            .generics
            .constraints
            .iter()
            .for_each(|constraint| {
                if !constraints.contains(constraint) {
                    constraints.insert(constraint.clone());
                };
                if let GenericConstraint::Lifetime(lifetime) = constraint {
                    for &supertype in &lifetime.bounds {
                        subtypes.insert(lifetime.lifetime, supertype);
                    }
                };
            })
    }

    fn set_output_equal_to_last_value(
        &self,
        constraints: &mut ConstraintSet,
        type_equality_sets: &mut TypeEqualitySets,
        subtypes: &mut LifetimeSubtypeMap,
    ) {
        // The type of the outgoing value must be the same as the return value
        if self.values.end.0 > self.values.start.0 {
            let return_value_type =
                VALUES.with_borrow(|values| values[self.values.end.0 - 1].get_type());

            type_equality_sets.insert_types_as_equal(
                self.f.sig.output.0.clone(),
                return_value_type.0,
                constraints,
                subtypes,
            )
        }
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
    type_equality_sets: TypeEqualitySets,
    transitive_closure: &TransitiveClosure,
) -> (BTreeSet<GenericParam>, ConcreteMapAndSets) {
    use TypeNode::*;
    let mut relevant_generic_params = BTreeSet::new();
    let most_concrete_type_map = BTreeMap::new();

    let mut concrete_maps_and_sets = ConcreteMapAndSets {
        most_concrete_type_map,
        type_equality_sets,
    };

    for param in original_generic_params.iter() {
        match param {
            GenericParam::Type(type_param) => {
                let type_param = param.type_param().unwrap();
                let set_ref = concrete_maps_and_sets
                    .type_equality_sets
                    .get_set_ref(&TypeParam(type_param));
                let set_ref = set_ref.unwrap_or_else(|| {
                    concrete_maps_and_sets
                        .type_equality_sets
                        .new_set(TypeParam(type_param))
                });

                let node =
                    set_ref.make_most_concrete(&mut concrete_maps_and_sets, transitive_closure);
                node.inner_params(
                    &mut concrete_maps_and_sets.type_equality_sets,
                    &mut relevant_generic_params,
                )
            }

            GenericParam::Lifetime(lifetime) => {
                let mut lifetime = *lifetime;
                lifetime.make_most_concrete(transitive_closure);
                relevant_generic_params.insert(GenericParam::Lifetime(lifetime));
            }

            GenericParam::Const(_) => unimplemented!(),
        }
    }

    (relevant_generic_params, concrete_maps_and_sets)
}

fn get_data_struct_args(
    original_data_struct_args: Vec<GenericParam>,
    concrete_maps_and_sets: &mut ConcreteMapAndSets,
    transitive_closure: &TransitiveClosure,
) -> GenericArguments {
    GenericArguments {
        args: original_data_struct_args
            .into_iter()
            .map(|arg| match arg {
                GenericParam::Type(ty) => {
                    let mut node = TypeNode::TypeParam(ty);
                    node.make_most_concrete(concrete_maps_and_sets, transitive_closure);
                    GenericArgument::Type(Type(node))
                }
                GenericParam::Lifetime(mut lifetime) => {
                    lifetime.make_most_concrete(transitive_closure);
                    GenericArgument::Lifetime(lifetime)
                }
                GenericParam::Const(_) => unimplemented!(),
            })
            .collect(),
    }
}

fn get_trait_args(
    original_trait_args: Vec<GenericArgument>,
    concrete_maps_and_sets: &mut ConcreteMapAndSets,
    transitive_closure: &TransitiveClosure,
) -> GenericArguments {
    GenericArguments {
        args: original_trait_args
            .into_iter()
            .map(|arg| match arg {
                GenericArgument::Type(mut ty) => {
                    ty.0.make_most_concrete(concrete_maps_and_sets, transitive_closure);
                    GenericArgument::Type(Type(ty.0))
                }

                GenericArgument::Lifetime(mut lifetime) => {
                    lifetime.make_most_concrete(transitive_closure);
                    GenericArgument::Lifetime(lifetime)
                }

                _ => unimplemented!(),
            })
            .collect(),
    }
}

impl GenericConstraint {
    /// This method tries to make a constraint that uses the correctly
    /// inferred types based on the analysis done so far. There is a chance
    /// that the constraint is not relevant at all, and thus it returns
    /// true if it is relevant, or false otherwise.
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
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        relevant_generic_params: &BTreeSet<GenericParam>,
        transitive_closure: &TransitiveClosure,
    ) -> bool {
        self.make_most_concrete(concrete_maps_and_sets, transitive_closure);
        self.is_relevant(
            &concrete_maps_and_sets.type_equality_sets,
            &relevant_generic_params,
        )
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

            GenericConstraint::Lifetime(lifetime) => {
                lifetime.is_relevant_for_constraint(relevant_generic_params)
            }
        }
    }

    fn make_most_concrete(
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) {
        match self {
            GenericConstraint::Type(pred_ty) => {
                pred_ty.make_most_concrete(concrete_maps_and_sets, transitive_closure)
            }
            GenericConstraint::Lifetime(lifetime) => {
                lifetime.make_most_concrete(transitive_closure)
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
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) {
        self.bounded_ty
            .0
            .make_most_concrete(concrete_maps_and_sets, transitive_closure);

        self.bounds.iter_mut().for_each(|bound| {
            bound.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure)
        });

        self.lifetimes.iter_mut().for_each(|lifetime| {
            lifetime.make_most_concrete(transitive_closure);
        });
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
            Reference {
                lifetime, inner, ..
            } => inner.is_relevant_for_constraint(type_equality_sets, relevant_generic_params),

            _ => false,
        }
    }

    fn make_most_concrete(
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) {
        if let Some(set_ref) = concrete_maps_and_sets.type_equality_sets.get_set_ref(self) {
            *self = set_ref.make_most_concrete(concrete_maps_and_sets, transitive_closure);
        }
    }

    /// Makes the most concrete TypeNode from two TypeNodes that are consdered
    /// to be equal. This is primarily decided based on the inner types of the
    /// nodes.
    fn make_most_concrete_from_pair(
        ty1: TypeNode,
        ty2: TypeNode,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) -> Self {
        use TypeNode::*;
        match (ty1, ty2) {
            (Infer, mut node) | (mut node, Infer) => {
                node.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure);
                node
            }
            (PrimitiveStr, _) | (_, PrimitiveStr) => PrimitiveStr,
            (Path(path1), Path(path2)) => crate::Path::make_most_concrete_from_pair(
                path1,
                path2,
                concrete_maps_and_sets,
                transitive_closure,
            ),
            (Path(mut path), _) | (_, Path(mut path)) => {
                path.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure);
                Path(path)
            }
            (Tuple(types1), Tuple(types2)) if types1.len() == types2.len() => Tuple(
                types1
                    .into_iter()
                    .zip(types2.into_iter())
                    .map(|(ty1, ty2)| {
                        Type(TypeNode::make_most_concrete_from_pair(
                            ty1.0,
                            ty2.0,
                            concrete_maps_and_sets,
                            transitive_closure,
                        ))
                    })
                    .collect(),
            ),
            (
                Reference {
                    is_mut: is_mut1,
                    lifetime: lifetime1,
                    inner: inner1,
                },
                Reference {
                    is_mut: is_mut2,
                    lifetime: lifetime2,
                    inner: inner2,
                },
            ) => Reference {
                is_mut: is_mut1 && is_mut2,

                inner: Box::new(TypeNode::make_most_concrete_from_pair(
                    *inner1,
                    *inner2,
                    concrete_maps_and_sets,
                    transitive_closure,
                )),
                lifetime: lifetime1
                    .and_then(|lifetime1| lifetime2.map(|lifetime2| lifetime1.min(lifetime2)))
                    .or(lifetime1)
                    .or(lifetime2),
            },
            (TraitObject(_), mut node) | (mut node, TraitObject(_)) => {
                node.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure);
                node
            }
            (TypeParam(ref1), TypeParam(ref2)) => TypeParam(ref1.min(ref2)),
            (TypeParam(_), mut node) | (mut node, TypeParam(_)) => {
                node.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure);
                node
            }
            (node1, node2) => panic!(
                "TypeNode: make_most_concrete_pair: incompatible types \n{:#?}\nand\n{:#?}",
                node1, node2
            ),
        }
    }

    fn make_most_concrete_inner(
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) {
        use TypeNode::*;
        match self {
            Tuple(types) => types.iter_mut().for_each(|ty| {
                ty.0.make_most_concrete(concrete_maps_and_sets, transitive_closure);
            }),
            Reference {
                inner, lifetime, ..
            } => {
                inner.make_most_concrete(concrete_maps_and_sets, transitive_closure);

                if let Some(lifetime) = lifetime {
                    lifetime.make_most_concrete(transitive_closure);
                }
            }
            Path(path) => path.make_most_concrete_inner(concrete_maps_and_sets, transitive_closure),
            node => {}
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

impl Lifetime {
    fn make_most_concrete(&mut self, transitive_closure: &TransitiveClosure) {
        transitive_closure.make_most_concrete_lifetime(self);
    }

    fn is_relevant_for_constraint(self, relevant_generic_params: &BTreeSet<GenericParam>) -> bool {
        self == STATIC_LIFETIME || relevant_generic_params.contains(&GenericParam::Lifetime(self))
    }
}

impl LifetimeDef {
    fn make_most_concrete(&mut self, transitive_closure: &TransitiveClosure) {
        self.lifetime.make_most_concrete(transitive_closure);

        self.bounds.iter_mut().for_each(|lifetime| {
            lifetime.make_most_concrete(transitive_closure);
        });
    }

    fn is_relevant_for_constraint(&self, relevant_generic_params: &BTreeSet<GenericParam>) -> bool {
        // The static lifetime is a subtype of all lifetimes so it redundant to say
        self.lifetime != STATIC_LIFETIME
            && self
                .lifetime
                .is_relevant_for_constraint(relevant_generic_params)
            && self
                .bounds
                .iter()
                .all(|lifetime| lifetime.is_relevant_for_constraint(relevant_generic_params))
            && if self.bounds.len() == 1 {
                self.lifetime != self.bounds[0]
            } else {
                true
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
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) {
        match self {
            TypeParamBound::Trait(bound) => {
                bound
                    .path
                    .make_most_concrete_inner(concrete_maps_and_sets, transitive_closure);
            }

            // FIXME: properly deal with lifetimes
            bound @ TypeParamBound::Lifetime(_) => {}
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
        for segment in &self.path {
            match &segment.args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    for arg in &args.args.args {
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
        &mut self,
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
    ) {
        let path_len = self.path.len();
        let segment = &mut self.path[path_len - 1];
        match &mut segment.args {
            PathArguments::None => {}
            PathArguments::AngleBracketed(args) => {
                args.args.args.iter_mut().for_each(|arg| match arg {
                    GenericArgument::Type(ty) => {
                        ty.0.make_most_concrete(concrete_maps_and_sets, transitive_closure)
                    }

                    GenericArgument::Lifetime(lifetime) => {
                        // FIXME: Lifetime
                        unimplemented!("Path::make_most_concrete_inner: Lifetime")
                    }
                    _ => unimplemented!(),
                });
            }
            PathArguments::Parenthesized(_) => {
                unimplemented!("Path::make_most_concrete_inner: Parenthesized")
            }
        };
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
        concrete_maps_and_sets: &mut ConcreteMapAndSets,
        transitive_closure: &TransitiveClosure,
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
                    Ordering::Less => {
                        let mut node = TypeNode::Path(path1);
                        node.make_most_concrete(concrete_maps_and_sets, transitive_closure);
                        node
                    }

                    Ordering::Greater => {
                        let mut node = TypeNode::Path(path2);
                        node.make_most_concrete(concrete_maps_and_sets, transitive_closure);
                        node
                    }

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
                                            concrete_maps_and_sets,
                                            transitive_closure,
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
