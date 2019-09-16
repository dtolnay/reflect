use crate::Invoke;
use crate::Push;
use crate::Signature;
use crate::Type;
use crate::Value;
use crate::ValueNode;
use crate::WIP;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<Type>,
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
            args: args.to_vec().into_iter().map(|value| value.index).collect(),
        });
        let node = ValueNode::Invoke(invoke);
        let value = Value {
            index: wip.values.index_push(node),
        };
        value
    }
}
