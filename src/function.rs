use crate::{Generics, Invoke, ParamMap, Push, Signature, Type, Value, ValueNode, WIP};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) content: Rc<FunctionContent>,
}

#[doc(hidden)]
#[derive(Debug, Clone)]
pub struct FunctionContent {
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

    #[doc(hidden)]
    pub fn new(content: Rc<FunctionContent>) -> Function {
        Function { content }
    }
}

impl FunctionContent {
    pub fn get_function(name: &str, sig: Signature) -> FunctionContent {
        FunctionContent {
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

    pub fn set_generics(&mut self, generics: Generics) {
        self.generics = Some(generics);
    }

    pub fn get_param_map(&self) -> Option<ParamMap> {
        self.generics
            .as_ref()
            .map(|generics| generics.get_param_map())
    }
}
