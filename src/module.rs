use crate::index::Push;
use crate::{MacroInvoke, Path, Type, TypeNode, Value, ValueNode, WIP};
use std::rc::Rc;
use std::str::FromStr;
use syn::parse::Result;
use syn::Error;

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
        Type(TypeNode::Path(self.get_path(name)))
    }

    pub fn invoke_macro(&self, name: &str, values: &[Value]) -> Value {
        let wip = WIP.with(Rc::clone);
        let wip = &mut *wip.borrow_mut();
        let wip = wip.as_mut().unwrap();

        let invoke = wip.macros.index_push(MacroInvoke {
            macro_path: self.path.get_path(name),
            args: values.iter().map(|value| value.index).collect(),
        });

        let node = ValueNode::MacroInvocation(invoke);
        Value {
            index: wip.values.index_push(node),
        }
    }
}

impl FromStr for Module {
    type Err = Error;
    // TODO: Consider using custum error type
    fn from_str(path: &str) -> Result<Self> {
        Ok(Module {
            path: Path::from_str(path)?,
        })
    }
}
