use crate::{Generics, ParamMap, Type};
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
    SelfByReference,
    SelfByReferenceMut,
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
        self.receiver = Receiver::SelfByReference;
    }

    pub fn set_self_by_reference_mut(&mut self) {
        self.receiver = Receiver::SelfByReferenceMut;
    }

    pub fn add_input(&mut self, input: Type) {
        self.inputs.push(input);
    }

    pub fn set_output(&mut self, output: Type) {
        self.output = output;
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> ParamMap {
        self.generics
            .get_or_insert(Default::default())
            .set_generic_params(params)
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str], param_map: &mut ParamMap) {
        self.generics
            .get_or_insert(Default::default())
            .set_generic_constraints(constraints, param_map);
    }
}
