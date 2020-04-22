use crate::{GlobalPush, MacroInvoke, Path, Value, ValueNode, MACROS, VALUES};

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

    pub fn invoke_macro(&self, name: &str, values: &[Value]) -> Value {
        let macro_path = self.path.get_simple_path(name);
        let invoke = MACROS.index_push(MacroInvoke {
            macro_path,
            args: values.iter().map(|value| value.index).collect(),
        });

        let node = ValueNode::MacroInvocation(invoke);
        Value {
            index: VALUES.index_push(node),
        }
    }
}
