use crate::Data;
use crate::Function;
use crate::Generics;
use crate::Signature;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Type(pub(crate) TypeNode);

#[derive(Debug, Clone)]
pub(crate) enum TypeNode {
    Infer,
    Unit,
    PrimitiveStr,
    Reference(Box<TypeNode>),
    ReferenceMut(Box<TypeNode>),
    Dereference(Box<TypeNode>),
    DataStructure {
        name: String,
        data: Data<TypeNode>,
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
        Type(TypeNode::Unit)
    }

    pub fn primitive_str() -> Self {
        Type(TypeNode::PrimitiveStr)
    }

    pub fn reference(&self) -> Self {
        Type(TypeNode::Reference(Box::new(self.0.clone())))
    }

    pub fn reference_mut(&self) -> Self {
        Type(TypeNode::ReferenceMut(Box::new(self.0.clone())))
    }

    pub fn dereference(&self) -> Self {
        match self.0 {
            TypeNode::Reference(ref inner) => Type((**inner).clone()),
            TypeNode::ReferenceMut(ref inner) => Type((**inner).clone()),
            ref other => Type(TypeNode::Dereference(Box::new(other.clone()))),
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
        match self.0 {
            TypeNode::DataStructure { ref data, .. } => {
                data.clone().map(|field| Type(field.element))
            }
            TypeNode::Reference(ref inner) => Type((**inner).clone())
                .data()
                .map(|field| field.element.reference()),
            TypeNode::ReferenceMut(ref inner) => Type((**inner).clone())
                .data()
                .map(|field| field.element.reference_mut()),
            _ => panic!("Type::data"),
        }
    }
}

impl TypeNode {
    pub(crate) fn get_name(&self) -> String {
        match *self {
            TypeNode::DataStructure { ref name, .. } => name.clone(),
            TypeNode::Reference(ref inner) => (&**inner).get_name(),
            TypeNode::ReferenceMut(ref inner) => (&**inner).get_name(),
            _ => panic!("Type::get_name"),
        }
    }
}
