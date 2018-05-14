use Generics;
use Type;

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
        Type::Path {
            global: self.global,
            path: self.path.clone(),
            name: name.to_owned(),
            generics: Generics { private: () },
        }
    }

    pub fn get_generic_type(&self, name: &str, generics: Generics) -> Type {
        Type::Path {
            global: self.global,
            path: self.path.clone(),
            name: name.to_owned(),
            generics,
        }
    }
}
