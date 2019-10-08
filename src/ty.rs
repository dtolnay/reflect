use crate::Data;
use crate::Function;
use crate::Ident;
use crate::Lifetime;
use crate::Path;
use crate::PathArguments;
use crate::PathSegment;
use crate::Signature;
use crate::TypeParamBound;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Type(pub(crate) TypeNode);

#[derive(Debug, Clone)]
pub(crate) enum TypeNode {
    Infer,
    Unit,
    PrimitiveStr,
    Reference {
        lifetime: Option<Lifetime>,
        inner: Box<TypeNode>,
    },
    ReferenceMut {
        lifetime: Option<Lifetime>,
        inner: Box<TypeNode>,
    },
    Dereference(Box<TypeNode>),
    TraitObject(Vec<TypeParamBound>),
    DataStructure {
        name: Ident,
        data: Data<TypeNode>,
    },
    Path(Path),
}

impl Type {
    pub fn unit() -> Self {
        Type(TypeNode::Unit)
    }

    pub fn primitive_str() -> Self {
        Type(TypeNode::PrimitiveStr)
    }

    pub fn reference(&self) -> Self {
        Type(TypeNode::Reference {
            lifetime: None,
            inner: Box::new(self.0.clone()),
        })
    }

    pub fn reference_mut(&self) -> Self {
        Type(TypeNode::ReferenceMut {
            lifetime: None,
            inner: Box::new(self.0.clone()),
        })
    }

    pub fn dereference(&self) -> Self {
        match self.0 {
            TypeNode::Reference { ref inner, .. } => Type((**inner).clone()),
            TypeNode::ReferenceMut { ref inner, .. } => Type((**inner).clone()),
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
            TypeNode::Reference {
                ref lifetime,
                ref inner,
            } => Type((**inner).clone()).data().map(|field| {
                Type(TypeNode::Reference {
                    lifetime: lifetime.clone(),
                    inner: Box::new(field.element.0.clone()),
                })
            }),
            TypeNode::ReferenceMut {
                ref lifetime,
                ref inner,
            } => Type((**inner).clone()).data().map(|field| {
                Type(TypeNode::ReferenceMut {
                    lifetime: lifetime.clone(),
                    inner: Box::new(field.element.0.clone()),
                })
            }),
            _ => panic!("Type::data"),
        }
    }

    pub(crate) fn syn_to_type(ty: &syn::Type) -> Type {
        match ty {
            syn::Type::Path(syn::TypePath {
                //FIXME add qself to Path
                qself: None,
                path:
                    syn::Path {
                        leading_colon,
                        segments,
                    },
            }) => {
                let path: Vec<_> = segments
                    .iter()
                    .map(|syn::PathSegment { ident, arguments }| {
                        let ident = Ident::from(ident.clone());
                        match arguments {
                            syn::PathArguments::None => PathSegment {
                                ident,
                                args: PathArguments::None,
                            },
                            //FIXME: generics
                            syn::PathArguments::AngleBracketed(_generic_args) => {
                                unimplemented!("Type::syn_to_type: angle bracketed generic args")
                            }

                            //FIXME: Generics
                            syn::PathArguments::Parenthesized(_parenthesized) => {
                                unimplemented!("Type::syn_to_type: parentesized generic args")
                            }
                        }
                    })
                    .collect();
                Type(TypeNode::Path(Path {
                    global: leading_colon.is_some(),
                    path,
                }))
            }
            syn::Type::Reference(reference) => {
                let inner = Box::new(Type::syn_to_type(&*reference.elem).0);
                let lifetime = reference.lifetime.as_ref().map(|lifetime| {
                    //FIXME: generics
                    unimplemented!("Type::syn_to_type: lifetime")
                });
                if reference.mutability.is_some() {
                    Type(TypeNode::ReferenceMut { lifetime, inner })
                } else {
                    Type(TypeNode::Reference { lifetime, inner })
                }
            }
            //FIXME: TraitObject
            syn::Type::TraitObject(type_trait_object) => {
                unimplemented!("Type::syn_to_type: TraitObject")
            }

            syn::Type::Tuple(type_tuple) => {
                if type_tuple.elems.is_empty() {
                    Type::unit()
                } else {
                    unimplemented!("Type::syn_to_type: type tuple")
                }
            }
            _ => unimplemented!("Type::syn_to_type"),
        }
    }
}

impl TypeNode {
    pub(crate) fn get_name(&self) -> String {
        match *self {
            TypeNode::DataStructure { ref name, .. } => name.to_string(),
            TypeNode::Reference { ref inner, .. } => (&**inner).get_name(),
            TypeNode::ReferenceMut { ref inner, .. } => (&**inner).get_name(),
            _ => panic!("Type::get_name"),
        }
    }
}
