use crate::{
    GenericArgument, GenericParam, Generics, GlobalCounter, Lifetime, ParamMap, Path,
    PathArguments, RefMap, Type,
    TypeNode::{self, *},
    TypeParamBound, LIFETIMES,
};
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
    SelfByReference {
        is_mut: bool,
        lifetime: OptionLifetime,
    },
}

#[derive(Debug, Clone, Copy)]
#[doc(hidden)]
pub struct OptionLifetime(pub(crate) Option<Lifetime>);

pub trait AddInput<'a, T> {
    fn add_input(&'a mut self, into_input: T);
}

pub trait SetOutput<'a, T> {
    fn set_output(&'a mut self, into_output: T);
}

impl Receiver {
    pub(crate) fn clone_with_fresh_generics(&self, ref_map: &RefMap) -> Self {
        use Receiver::*;
        match *self {
            NoSelf => NoSelf,
            SelfByValue => SelfByValue,
            SelfByReference { is_mut, lifetime } => SelfByReference {
                is_mut,
                lifetime: OptionLifetime(Some(
                    lifetime.0.unwrap().clone_with_fresh_generics(ref_map),
                )),
            },
        }
    }
}

impl<'a> AddInput<'a, Type> for Signature {
    fn add_input(&'a mut self, into_input: Type) {
        self.inputs.push(into_input)
    }
}

impl<'a, F> AddInput<'a, F> for Signature
where
    F: FnOnce(&'a mut ParamMap) -> Type,
{
    fn add_input(&'a mut self, into_input: F) {
        self.inputs.push((into_input)(&mut self.generics.param_map))
    }
}

impl<'a> SetOutput<'a, Type> for Signature {
    fn set_output(&'a mut self, into_output: Type) {
        self.output = into_output;
    }
}

impl<'a, F> SetOutput<'a, F> for Signature
where
    F: FnOnce(&'a mut ParamMap) -> Type,
{
    fn set_output(&'a mut self, into_output: F) {
        self.output = (into_output)(&mut self.generics.param_map);
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
        self.receiver = Receiver::SelfByReference {
            is_mut: false,
            lifetime: OptionLifetime(None),
        };
    }

    pub fn set_self_by_reference_mut(&mut self) {
        self.receiver = Receiver::SelfByReference {
            is_mut: true,
            lifetime: OptionLifetime(None),
        };
    }

    /// Add input type to signature.
    /// T can be either a Type or a type implementing FnOnce(&'a mut ParamMap) -> Type
    pub fn add_input<'a, T>(&'a mut self, into_input: T)
    where
        Self: AddInput<'a, T>,
    {
        <Self as AddInput<'a, T>>::add_input(self, into_input);
    }

    /// Set output type to signature.
    /// T can be either a Type or a type implementing FnOnce(&'a mut ParamMap) -> Type
    pub fn set_output<'a, T>(&'a mut self, into_output: T)
    where
        Self: SetOutput<'a, T>,
    {
        <Self as SetOutput<'a, T>>::set_output(self, into_output);
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> &mut ParamMap {
        self.generics.set_generic_params(params)
    }

    pub fn add_parent_params(&mut self, param_map: &mut ParamMap) {
        self.generics.param_map.append(param_map);
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str]) {
        self.generics.set_generic_constraints(constraints);
    }

    /// Explicitly insert elided lifetimes
    /// Should be called by Function::get_function after the all paramters are inserted
    pub(crate) fn insert_elided_lifetimes(&mut self) {
        use Receiver::*;
        let generics = &mut self.generics;
        // We need to insert the elided lifetimes first in the params so we
        // temporarily swap the params with an empty Vec, and then extend that
        // Vec with the old params in the end
        let params = std::mem::replace(&mut generics.params, Vec::new());

        match &mut self.receiver {
            NoSelf => {
                for ty in &mut self.inputs {
                    ty.0.insert_new_lifetimes(&mut generics.params);
                }
                if self.inputs.len() == 1 {
                    match &mut self.inputs[0].0 {
                        Reference {
                            lifetime: Some(lifetime),
                            inner,
                            ..
                        } if !inner.has_lifetimes() => self
                            .output
                            .0
                            .insert_new_lifetimes2(*lifetime, &mut generics.params),
                        _ => {}
                    }
                } else {
                    self.output.0.insert_new_lifetimes(&mut generics.params);
                }
            }
            SelfByValue => {
                for ty in &mut self.inputs {
                    ty.0.insert_new_lifetimes(&mut generics.params);
                }
                self.output.0.insert_new_lifetimes(&mut generics.params);
            }
            SelfByReference {
                lifetime: option_lifetime,
                ..
            } => {
                let lifetime = if let Some(lifetime) = option_lifetime.0 {
                    lifetime
                } else {
                    let lifetime = LIFETIMES.count();
                    generics.params.push(GenericParam::Lifetime(lifetime));
                    lifetime
                };
                option_lifetime.0 = Some(lifetime);
                for ty in &mut self.inputs {
                    ty.0.insert_new_lifetimes(&mut generics.params);
                }
                self.output
                    .0
                    .insert_new_lifetimes2(lifetime, &mut generics.params);
            }
        }
        // Insert the old params back into place
        generics.params.extend(params);
    }
}

impl TypeNode {
    fn insert_new_lifetimes(&mut self, params: &mut Vec<GenericParam>) {
        match self {
            Reference {
                inner, lifetime, ..
            } => {
                if lifetime.is_none() {
                    let new_lifetime = LIFETIMES.count();
                    params.push(GenericParam::Lifetime(new_lifetime));
                    *lifetime = Some(new_lifetime)
                };
                inner.insert_new_lifetimes(params);
            }

            Tuple(types) => {
                for ty in types.iter_mut() {
                    ty.0.insert_new_lifetimes(params);
                }
            }
            Dereference(node) => node.insert_new_lifetimes(params),
            TraitObject(bounds) => {
                for bound in bounds.iter_mut() {
                    if let TypeParamBound::Trait(bound) = bound {
                        bound.path.insert_new_lifetimes(params);
                    }
                }
            }
            Path(path) => path.insert_new_lifetimes(params),
            _ => {}
        }
    }

    fn insert_new_lifetimes2(&mut self, new_lifetime: Lifetime, params: &mut Vec<GenericParam>) {
        match self {
            Reference {
                inner, lifetime, ..
            } => {
                if lifetime.is_none() {
                    *lifetime = Some(new_lifetime);
                };
                inner.insert_new_lifetimes(params);
            }
            Tuple(types) => {
                for ty in types.iter_mut() {
                    ty.0.insert_new_lifetimes2(new_lifetime, params);
                }
            }
            Dereference(node) => node.insert_new_lifetimes2(new_lifetime, params),
            TraitObject(bounds) => {
                for bound in bounds.iter_mut() {
                    if let TypeParamBound::Trait(bound) = bound {
                        bound.path.insert_new_lifetimes2(new_lifetime, params);
                    }
                }
            }
            Path(path) => path.insert_new_lifetimes2(new_lifetime, params),
            _ => {}
        }
    }

    fn has_lifetimes(&self) -> bool {
        match self {
            Reference { .. } => true,
            Tuple(types) => types.iter().any(|ty| ty.0.has_lifetimes()),
            Dereference(node) => node.has_lifetimes(),
            TraitObject(bounds) => bounds.iter().any(|bound| match bound {
                TypeParamBound::Trait(bound) => bound.path.has_lifetimes(),
                TypeParamBound::Lifetime(_) => true,
            }),
            Path(path) => path.has_lifetimes(),
            _ => false,
        }
    }
}

impl Path {
    fn insert_new_lifetimes(&mut self, params: &mut Vec<GenericParam>) {
        for segment in &mut self.path {
            match &mut segment.args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    for arg in &mut args.args.args {
                        if let GenericArgument::Type(ty) = arg {
                            ty.0.insert_new_lifetimes(params)
                        }
                    }
                }
                PathArguments::Parenthesized(args) => {
                    unimplemented!("Path::insert_elided_lifetimes: PathArguments::Parenthesized")
                }
            }
        }
    }

    fn insert_new_lifetimes2(&mut self, lifetime: Lifetime, params: &mut Vec<GenericParam>) {
        for segment in &mut self.path {
            match &mut segment.args {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    for arg in &mut args.args.args {
                        if let GenericArgument::Type(ty) = arg {
                            ty.0.insert_new_lifetimes2(lifetime, params)
                        }
                    }
                }
                PathArguments::Parenthesized(args) => {
                    unimplemented!("Path::insert_elided_lifetimes2: PathArguments::Parenthesized")
                }
            }
        }
    }

    fn has_lifetimes(&self) -> bool {
        self.path.iter().any(|segment| match &segment.args {
            PathArguments::None => false,
            PathArguments::AngleBracketed(args) => args.args.args.iter().any(|arg| match arg {
                GenericArgument::Type(ty) => ty.0.has_lifetimes(),
                GenericArgument::Lifetime(_) => true,
                _ => unimplemented!(),
            }),
            PathArguments::Parenthesized(args) => {
                unimplemented!("Path::has_lifetimes: PathArguments::Parenthesized")
            }
        })
    }
}
