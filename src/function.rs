use crate::{Generics, Invoke, Push, Signature, Type, Value, ValueNode, WIP};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<ParentImpl>,
    pub(crate) name: String,
    pub(crate) sig: Signature,
}

#[derive(Debug, Clone)]
pub struct ParentImpl {
    pub(crate) ty: Type,
    pub(crate) generics: Option<Generics>,
}

impl Function {
    pub fn invoke(&self, args: &[Value]) -> Value {
        let wip = WIP.with(Rc::clone);
        let wip = &mut *wip.borrow_mut();
        let wip = wip.as_mut().unwrap();

        let invoke = wip.invokes.index_push(Invoke {
            function: self.clone(),
            args: args.iter().map(|value| value.index).collect(),
        });
        let node = ValueNode::Invoke(invoke);
        Value {
            index: wip.values.index_push(node),
        }
    }

    pub fn get_function(name: &str, sig: Signature) -> Function {
        Function {
            parent: None,
            name: name.to_owned(),
            sig,
        }
    }

    pub fn set_parent(&mut self, parent: ParentImpl) {
        self.parent = Some(parent);
    }
}

impl ParentImpl {
    pub fn new(ty: Type) -> Self {
        Self { ty, generics: None }
    }

    pub fn set_generic_params(&mut self, params: &[&str]) {
        self.generics
            .get_or_insert(Generics {
                params: Vec::new(),
                constraints: Vec::new(),
            })
            .set_generic_params(params);
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str]) {
        self.generics
            .get_or_insert(Generics {
                params: Vec::new(),
                constraints: Vec::new(),
            })
            .set_generic_constraints(constraints);
    }
}
