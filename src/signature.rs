use crate::{
    GenericArgument, GenericParam, Generics, GlobalCounter, Lifetime, ParamMap, Path,
    PathArguments, Type, TypeNode, TypeParamBound, LIFETIMES,
};
use std::collections::BTreeMap;
use std::default::Default;

#[derive(Debug, Clone)]
pub struct Signature {
    pub(crate) generics: Generics,
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
pub struct OptionLifetime(pub(crate) Option<Lifetime>);

impl Receiver {
    pub(crate) fn clone_with_fresh_generics(
        &self,
        ref_map: &BTreeMap<GenericParam, GenericParam>,
    ) -> Self {
        use Receiver::*;
        match *self {
            NoSelf => NoSelf,
            SelfByValue => SelfByValue,
            SelfByReference(lifetime) => SelfByReference(OptionLifetime(
                ref_map
                    .get(&GenericParam::Lifetime(lifetime.0.unwrap()))
                    .map(|param| param.lifetime())
                    .unwrap(),
            )),
            SelfByReferenceMut(lifetime) => SelfByReferenceMut(OptionLifetime(
                ref_map
                    .get(&GenericParam::Lifetime(lifetime.0.unwrap()))
                    .map(|param| param.lifetime())
                    .unwrap(),
            )),
        }
    }
}

impl Signature {
    pub fn new() -> Self {
        Signature {
            generics: Generics::default(),
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

    pub fn add_input<F>(&mut self, into_ty: F)
    where
        F: FnOnce(&mut ParamMap) -> Type,
    {
        self.inputs.push((into_ty)(&mut self.generics.param_map));
    }

    pub fn set_output<F>(&mut self, into_ty: F)
    where
        F: FnOnce(&mut ParamMap) -> Type,
    {
        self.output = (into_ty)(&mut self.generics.param_map);
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> &mut ParamMap {
        self.generics.set_generic_params(params)
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str]) {
        self.generics.set_generic_constraints(constraints);
    }

    /// Explicitly insert elided lifetimes
    /// Should be called by Function::get_function after the all paramters are inserted
    pub(crate) fn insert_elided_lifetimes(&mut self) {
        use Receiver::*;
        use TypeNode::*;
        let mut generics = &mut self.generics;
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
                            lifetime: Some(lifetime),
                            inner,
                        } if !inner.is_reference() => self
                            .output
                            .0
                            .insert_new_lifetimes2(*lifetime, &mut generics),
                        ReferenceMut {
                            lifetime: Some(lifetime),
                            inner,
                        } if !inner.is_reference() => self
                            .output
                            .0
                            .insert_new_lifetimes2(*lifetime, &mut generics),
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
            SelfByReference(option_lifetime) | SelfByReferenceMut(option_lifetime) => {
                let lifetime = if let Some(lifetime) = option_lifetime.0 {
                    lifetime
                } else {
                    let lifetime = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(lifetime));
                    lifetime
                };
                option_lifetime.0 = Some(lifetime);
                for ty in self.inputs.iter_mut() {
                    ty.0.insert_new_lifetimes(&mut generics);
                }
                self.output.0.insert_new_lifetimes2(lifetime, &mut generics);
            }
        }
        // Insert the old params back into place
        generics.params.extend(params);
    }
}

impl TypeNode {
    fn insert_new_lifetimes(&mut self, generics: &mut Generics) {
        use TypeNode::*;
        match self {
            Reference { inner, lifetime } => {
                if lifetime.is_none() {
                    let new_lifetime = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(new_lifetime));
                    *lifetime = Some(new_lifetime)
                };
                inner.insert_new_lifetimes(generics);
            }
            ReferenceMut { inner, lifetime } => {
                if lifetime.is_none() {
                    let new_lifetime = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(new_lifetime));
                    *lifetime = Some(new_lifetime)
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

    fn insert_new_lifetimes2(&mut self, new_lifetime: Lifetime, generics: &mut Generics) {
        use TypeNode::*;
        match self {
            Reference { inner, lifetime } => {
                if lifetime.is_none() {
                    *lifetime = Some(new_lifetime);
                };
                inner.insert_new_lifetimes(generics);
            }
            ReferenceMut { inner, lifetime } => {
                if lifetime.is_none() {
                    *lifetime = Some(new_lifetime);
                };
                inner.insert_new_lifetimes(generics);
            }
            Tuple(types) => {
                for ty in types.iter_mut() {
                    ty.0.insert_new_lifetimes2(new_lifetime, generics);
                }
            }
            Dereference(node) => node.insert_new_lifetimes2(new_lifetime, generics),
            TraitObject(bounds) => {
                for bound in bounds.iter_mut() {
                    if let TypeParamBound::Trait(bound) = bound {
                        bound.path.insert_new_lifetimes2(new_lifetime, generics);
                    }
                }
            }
            Path(path) => path.insert_new_lifetimes2(new_lifetime, generics),
            _ => {}
        }
    }

    fn is_reference(&self) -> bool {
        use TypeNode::*;
        match self {
            Reference { .. } | ReferenceMut { .. } => true,
            _ => false,
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

    fn insert_new_lifetimes2(&mut self, lifetime: Lifetime, generics: &mut Generics) {
        for segment in self.path.iter_mut() {
            match &mut segment.args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    for arg in args.args.args.iter_mut() {
                        if let GenericArgument::Type(ty) = arg {
                            ty.0.insert_new_lifetimes2(lifetime, generics)
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
