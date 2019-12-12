use crate::{Generics, Invoke, ParamMap, Push, Signature, Type, Value, ValueNode, WIP};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<Rc<Parent>>,
    pub(crate) name: String,
    pub(crate) sig: Signature,
}

#[derive(Debug, Clone)]
pub struct Parent {
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

    /// When calling set_parent it is important to use a reference to the same
    /// Parent struct for all functions declared inside of the same impl or
    /// trait definition. Otherwise the trait inference may not work correctly
    pub fn set_parent(&mut self, parent: Rc<Parent>) {
        self.parent = Some(parent);
    }
}

impl Parent {
    pub fn new(ty: Type) -> Self {
        Self { ty, generics: None }
    }

    pub fn set_generic_params(&mut self, params: &[&str]) -> ParamMap {
        self.generics
            .get_or_insert(Generics {
                params: Vec::new(),
                constraints: Vec::new(),
            })
            .set_generic_params(params)
    }

    pub fn set_generic_constraints(&mut self, constraints: &[&str], param_map: &mut ParamMap) {
        self.generics
            .get_or_insert(Generics {
                params: Vec::new(),
                constraints: Vec::new(),
            })
            .set_generic_constraints(constraints, param_map);
    }

    pub fn set_type_params(&mut self, params: &[&str], param_map: &mut ParamMap) {
        self.ty.set_params(params, param_map)
    }

    pub fn get_param_map(&self) -> Option<ParamMap> {
        self.generics
            .as_ref()
            .map(|generics| generics.get_param_map())
    }
}
