use crate::Ident;
use crate::Path;
use crate::PathArguments;
use crate::PathSegment;
use crate::Type;
use crate::TypeNode;
use std::iter::once;

#[derive(Debug, Clone)]
pub struct Module {
    pub(crate) global: bool,
    pub(crate) path: Vec<String>,
}

impl Module {
    pub fn root() -> Self {
        Module {
            global: true,
            path: Vec::new(),
        }
    }

    pub fn get_module(&self, name: &str) -> Module {
        let mut path = self.path.clone();
        path.push(name.to_owned());
        Module {
            global: self.global,
            path,
        }
    }

    pub fn get_type(&self, name: &str) -> Type {
        Type(TypeNode::Path(Path {
            global: self.global,
            path: self
                .path
                .iter()
                .chain(once(&name.to_owned()))
                .map(|segment| PathSegment {
                    ident: Ident::new(segment),
                    args: PathArguments::None,
                })
                .collect(),
        }))
    }
}
