use crate::Array;
use crate::Invoke;
use crate::Push;
use crate::Signature;
use crate::Type;
use crate::Value;
use crate::ValueNode;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<Type>,
    pub(crate) name: String,
    pub(crate) sig: Signature,
}

impl Function {
    pub fn invoke<'a, Args: Array<Value<'a>>>(&self, args: Args) -> Value<'a> {
        let wip = args.first().function;
        let invoke = wip.invokes.borrow_mut().index_push(Invoke {
            function: self.clone(),
            args: args.to_vec().into_iter().map(|value| value.index).collect(),
        });
        let node = ValueNode::Invoke(invoke);
        Value {
            function: wip,
            index: wip.values.borrow_mut().index_push(node),
        }
    }
}
