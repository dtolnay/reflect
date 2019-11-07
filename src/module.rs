use crate::Path;
use crate::Type;
use crate::TypeNode;

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
}
