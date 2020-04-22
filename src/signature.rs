use crate::{
    GenericArgument, GenericParam, Generics, GlobalCounter, LifetimeRef, ParamMap, Path,
    PathArguments, Type, TypeNode, TypeParamBound, LIFETIMES,
};
use std::collections::BTreeMap;
use std::default::Default;

#[derive(Debug, Clone)]
pub struct Signature {
    pub(crate) generics: Option<Generics>,
    pub(crate) receiver: Receiver,
    pub(crate) inputs: Vec<Type>,
    pub(crate) output: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum Receiver {
    NoSelf,
    SelfByValue,
    SelfByReference(OptionLifetime),
    SelfByReferenceMut(OptionLifetime),
}

#[derive(Debug, Clone, Copy)]
#[doc(hidden)]
pub struct OptionLifetime(pub(crate) Option<LifetimeRef>);

impl Receiver {
    pub(crate) fn clone_with_fresh_generics(
        &self,
        ref_map: &BTreeMap<GenericParam, GenericParam>,
    ) -> Self {
        use Receiver::*;
        match *self {
            NoSelf => NoSelf,
            SelfByValue => SelfByValue,
            SelfByReference(lifetime_ref) => SelfByReference(OptionLifetime(
                ref_map
                    .get(&GenericParam::Lifetime(lifetime_ref.0.unwrap()))
                    .map(|param| param.lifetime_ref())
                    .unwrap(),
            )),
            SelfByReferenceMut(lifetime_ref) => SelfByReferenceMut(OptionLifetime(
                ref_map
                    .get(&GenericParam::Lifetime(lifetime_ref.0.unwrap()))
                    .map(|param| param.lifetime_ref())
                    .unwrap(),
            )),
        }
    }
}

pub trait AddInput<T> {
    fn add_input(&mut self, into_ty: T);
}

impl AddInput<Type> for Signature {
    fn add_input(&mut self, into_ty: Type) {
        self.inputs.push(into_ty);
    }
}

impl<F> AddInput<F> for Signature
where
    F: Fn(&mut ParamMap) -> Type,
{
    fn add_input(&mut self, into_ty: F) {
        self.inputs
            .push((into_ty)(&mut self.generics.as_mut().unwrap().param_map));
    }
}

pub trait SetOutput<T> {
    fn set_output(&mut self, into_ty: T);
}

impl SetOutput<Type> for Signature {
    fn set_output(&mut self, into_ty: Type) {
        self.output = into_ty;
    }
}

impl<F> SetOutput<F> for Signature
where
    F: Fn(&mut ParamMap) -> Type,
{
    fn set_output(&mut self, into_ty: F) {
        self.output = (into_ty)(&mut self.generics.as_mut().unwrap().param_map);
    }
}

impl Signature {
    pub fn new() -> Self {
        Signature {
            generics: None,
            receiver: Receiver::NoSelf,
            inputs: Vec::new(),
            output: Type::unit(),
        }
    }

    pub fn set_self_by_value(&mut self) {
        self.receiver = Receiver::SelfByValue;
    }

    pub fn set_self_by_reference(&mut self) {
        self.receiver = Receiver::SelfByReference(OptionLifetime(None));
    }

    pub fn set_self_by_reference_mut(&mut self) {
        self.receiver = Receiver::SelfByReferenceMut(OptionLifetime(None));
    }

    pub fn add_input<T>(&mut self, input: T)
    where
        Self: AddInput<T>,
    {
        <Self as AddInput<T>>::add_input(self, input);
    }

    pub fn set_output<T>(&mut self, output: T)
    where
        Self: SetOutput<T>,
    {
        <Self as SetOutput<T>>::set_output(self, output);
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> &mut ParamMap {
        self.generics
            .get_or_insert(Default::default())
            .set_generic_params(params)
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str]) {
        let generics = self.generics.get_or_insert(Default::default());
        generics.set_generic_constraints(constraints);
    }

    /// Explicitly insert elided lifetimes
    /// Should be called by Function::get_function after the all paramters are inserted
    pub(crate) fn insert_elided_lifetimes(&mut self) {
        use Receiver::*;
        use TypeNode::*;
        // TODO: How does elsion rules work for generic traits with references as parameters?
        let mut generics = self.generics.get_or_insert(Default::default());
        // We need to insert the elided lifetimes first in the params so we
        // temporarily swap the params with an empty Vec, and then extend that
        // Vec with the old params in the end
        let params = std::mem::replace(&mut generics.params, Vec::new());

        match &mut self.receiver {
            NoSelf => {
                for ty in self.inputs.iter_mut() {
                    ty.0.insert_new_lifetimes(&mut generics);
                }
                if self.inputs.len() == 1 {
                    match &mut self.inputs[0].0 {
                        Reference {
                            lifetime: Some(lifetime_ref),
                            ..
                        } => self
                            .output
                            .0
                            .insert_new_lifetimes2(*lifetime_ref, &mut generics),
                        ReferenceMut {
                            lifetime: Some(lifetime_ref),
                            ..
                        } => self
                            .output
                            .0
                            .insert_new_lifetimes2(*lifetime_ref, &mut generics),
                        _ => {}
                    }
                } else {
                    self.output.0.insert_new_lifetimes(&mut generics);
                }
            }
            SelfByValue => {
                for ty in self.inputs.iter_mut() {
                    ty.0.insert_new_lifetimes(&mut generics);
                }
                self.output.0.insert_new_lifetimes(&mut generics);
            }
            SelfByReference(lifetime) | SelfByReferenceMut(lifetime) => {
                let lifetime_ref = if let Some(lifetime_ref) = lifetime.0 {
                    lifetime_ref
                } else {
                    let lifetime_ref = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(lifetime_ref));
                    lifetime_ref
                };
                lifetime.0 = Some(lifetime_ref);
                for ty in self.inputs.iter_mut() {
                    ty.0.insert_new_lifetimes(&mut generics);
                }
                self.output
                    .0
                    .insert_new_lifetimes2(lifetime_ref, &mut generics);
            }
        }
        // Insert the old params back into place
        generics.params.extend(params);

        if generics.params.is_empty() {
            self.generics = None;
        }
    }
}

impl TypeNode {
    fn insert_new_lifetimes(&mut self, generics: &mut Generics) {
        use TypeNode::*;
        match self {
            Reference { inner, lifetime } => {
                if lifetime.is_none() {
                    let lifetime_ref = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(lifetime_ref));
                    *lifetime = Some(lifetime_ref)
                };
                inner.insert_new_lifetimes(generics);
            }
            ReferenceMut { inner, lifetime } => {
                if lifetime.is_none() {
                    let lifetime_ref = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(lifetime_ref));
                    *lifetime = Some(lifetime_ref)
                };
                inner.insert_new_lifetimes(generics);
            }
            Tuple(types) => {
                for ty in types.iter_mut() {
                    ty.0.insert_new_lifetimes(generics);
                }
            }
            Dereference(node) => node.insert_new_lifetimes(generics),
            TraitObject(bounds) => {
                for bound in bounds.iter_mut() {
                    if let TypeParamBound::Trait(bound) = bound {
                        bound.path.insert_new_lifetimes(generics);
                    }
                }
            }
            Path(path) => path.insert_new_lifetimes(generics),
            _ => {}
        }
    }

    fn insert_new_lifetimes2(&mut self, lifetime_ref: LifetimeRef, generics: &mut Generics) {
        use TypeNode::*;
        match self {
            Reference { inner, lifetime } => {
                if lifetime.is_none() {
                    *lifetime = Some(lifetime_ref);
                };
                inner.insert_new_lifetimes(generics);
            }
            ReferenceMut { inner, lifetime } => {
                if lifetime.is_none() {
                    *lifetime = Some(lifetime_ref);
                };
                inner.insert_new_lifetimes(generics);
            }
            Tuple(types) => {
                for ty in types.iter_mut() {
                    ty.0.insert_new_lifetimes2(lifetime_ref, generics);
                }
            }
            node => node.insert_new_lifetimes(generics),
        }
    }
}

impl Path {
    fn insert_new_lifetimes(&mut self, generics: &mut Generics) {
        for segment in self.path.iter_mut() {
            match &mut segment.args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    for arg in args.args.args.iter_mut() {
                        if let GenericArgument::Type(ty) = arg {
                            ty.0.insert_new_lifetimes(generics)
                        }
                    }
                }
                PathArguments::Parenthesized(args) => {
                    unimplemented!("Path::insert_elided_lifetimes: PathArguments::Parenthesized")
                }
            }
        }
    }
}
