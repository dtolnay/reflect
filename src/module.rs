use crate::index::Push;
use crate::Path;
use crate::Type;
use crate::TypeNode;
use crate::WIP;
use crate::{MacroInvoke, Value, ValueNode};

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) path: Path,
}

impl Module {
    pub fn root() -> Self {
        Module {
            path: Path {
                path: Vec::new(),
                global: true,
            },
        }
    }

    pub fn get_module(&self, name: &str) -> Module {
        Module {
            path: self.path.get_path(name),
        }
    }

    pub fn get_path(&self, name: &str) -> Path {
        self.path.get_path(name)
    }

    pub fn get_type(&self, name: &str) -> Type {
        Type(TypeNode::Path(self.path.get_path(name)))
    }

    pub fn invoke_macro(&self, name: &str, values: &[Value]) -> Value {
        let wip = WIP.with(Rc::clone);
        let wip = &mut *wip.borrow_mut();
        let wip = wip.as_mut().unwrap();

        let invoke = wip.macros.index_push(MacroInvoke {
            macro_name: name.to_owned(),
            args: values.into_iter().map(|value| value.index).collect(),
        });

        let node = ValueNode::MacroInvocation(invoke);
        let value = Value {
            index: wip.values.index_push(node),
        };
        value
    }
}
