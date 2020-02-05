use crate::{Invoke, Parent, Push, Signature, Value, ValueNode, WIP};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<Rc<Parent>>,
    pub(crate) name: String,
    pub(crate) sig: Signature,
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
