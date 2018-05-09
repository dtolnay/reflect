use Data;
use Function;
use Generics;
use Signature;

#[derive(Debug, Clone)]
pub enum Type {
    Infer,
    Unit,
    PrimitiveStr,
    Reference(Box<Type>),
    ReferenceMut(Box<Type>),
    Dereference(Box<Type>),
    DataStructure {
        name: String,
        data: Data<Type>,
    },
    Path {
        global: bool,
        path: Vec<String>,
        name: String,
        generics: Generics,
    },
}

impl Type {
    pub fn unit() -> Self {
        Type::Unit
    }

    pub fn primitive_str() -> Self {
        Type::PrimitiveStr
    }

    pub fn reference(&self) -> Self {
        Type::Reference(Box::new(self.clone()))
    }

    pub fn reference_mut(&self) -> Self {
        Type::ReferenceMut(Box::new(self.clone()))
    }

    pub fn dereference(&self) -> Self {
        match *self {
            Type::Reference(ref inner) => (**inner).clone(),
            Type::ReferenceMut(ref inner) => (**inner).clone(),
            ref other => Type::Dereference(Box::new(other.clone())),
        }
    }

    pub fn get_function(&self, name: &str, sig: Signature) -> Function {
        Function {
            parent: Some(self.clone()),
            name: name.to_owned(),
            sig,
        }
    }

    pub fn data(&self) -> Data<Type> {
        match *self {
            Type::DataStructure { ref data, .. } => data.clone(),
            Type::Reference(ref inner) => (**inner)
                .clone()
                .data()
                .map(|field| field.element.reference()),
            Type::ReferenceMut(ref inner) => (**inner)
                .clone()
                .data()
                .map(|field| field.element.reference_mut()),
            _ => panic!("Type::data"),
        }
    }
}

impl Type {
    pub(crate) fn get_name(&self) -> String {
        match *self {
            Type::DataStructure { ref name, .. } => name.clone(),
            Type::Reference(ref inner) => (**inner).get_name(),
            Type::ReferenceMut(ref inner) => (**inner).get_name(),
            _ => panic!("Type::get_name"),
        }
    }
}
