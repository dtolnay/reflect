use crate::execution::WIP;
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
    pub fn invoke(&self, args: &[Value]) -> Value {
        let invoke = WIP.with(|wip| {
            wip.borrow()
                .as_ref()
                .unwrap()
                .invokes
                .borrow_mut()
                .index_push(Invoke {
                    function: self.clone(),
                    args: args.to_vec().into_iter().map(|value| value.index).collect(),
                })
        });
        let node = ValueNode::Invoke(invoke);
        Value {
            index: WIP.with(|wip| {
                wip.borrow()
                    .as_ref()
                    .unwrap()
                    .values
                    .borrow_mut()
                    .index_push(node)
            }),
        }
    }
}
