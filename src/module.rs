use crate::index::Push;
use crate::{GlobalBorrow, MacroInvoke, Path, Type, TypeNode, Value, ValueNode, GLOBAL_DATA};

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
            path: self.path.get_simple_path(name),
        }
    }

    /// Get a simple path without any generics
    pub fn get_simple_path(&self, name: &str) -> Path {
        self.path.get_simple_path(name)
    }

    pub fn get_simple_type(&self, name: &str) -> Type {
        Type(TypeNode::Path(self.get_simple_path(name)))
    }

    pub fn invoke_macro(&self, name: &str, values: &[Value]) -> Value {
        let macro_path = self.path.get_simple_path(name);
        let invoke = GLOBAL_DATA.with_borrow_macros_mut(|macros| {
            macros.index_push(MacroInvoke {
                macro_path,
                args: values.iter().map(|value| value.index).collect(),
            })
        });

        let node = ValueNode::MacroInvocation(invoke);
        Value {
            index: node.index_push(),
        }
    }
}
