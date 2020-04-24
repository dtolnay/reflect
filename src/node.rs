use crate::{
    Accessor, Data, GlobalBorrow, Ident, InvokeRef, MacroInvokeRef, Type, TypeNode, ValueRef,
    INVOKES,
};

#[derive(Debug, Clone)]
pub(crate) enum ValueNode {
    Tuple(Vec<ValueRef>),
    Str(String),
    // TODO: Add lifetime parameter
    Reference(ValueRef),
    // TODO: Add lifetime parameter
    ReferenceMut(ValueRef),
    Dereference(ValueRef),
    Binding {
        name: Ident,
        ty: Type,
    },
    DataStructure {
        name: String,
        data: Data<ValueRef>,
    },
    Invoke(InvokeRef),
    Destructure {
        parent: ValueRef,
        accessor: Accessor,
        ty: Type,
    },
    MacroInvocation(MacroInvokeRef),
}

impl ValueNode {
    // FIXME: Not safe to call when a node has been moved outside a WipFunction
    pub fn get_type(&self) -> Type {
        match self {
            ValueNode::Tuple(types) => Type(TypeNode::Tuple(
                types
                    .iter()
                    .map(|type_ref| type_ref.node().get_type())
                    .collect(),
            )),
            ValueNode::Str(_) => Type(TypeNode::PrimitiveStr),
            ValueNode::Reference(v) => Type(TypeNode::Reference {
                lifetime: None,
                inner: Box::new(v.node().get_type().0),
            }),
            ValueNode::ReferenceMut(v) => Type(TypeNode::ReferenceMut {
                lifetime: None,
                inner: Box::new(v.node().get_type().0),
            }),
            ValueNode::Binding { ty, .. } => ty.clone(),
            ValueNode::Destructure {
                parent,
                accessor,
                ty,
            } => ty.clone(),
            ValueNode::Invoke(invoke_ref) => {
                INVOKES.with_borrow(|invokes| invokes[invoke_ref.0].function.sig.output.clone())
            }

            node => panic!("ValueNode::get_type"),
        }
    }

    pub fn get_type_name(&self) -> Self {
        match self {
            ValueNode::Tuple(types) => {
                let types: String = types.iter().fold(String::from(""), |mut acc, v| {
                    match &v.node().get_type_name() {
                        ValueNode::Str(name) => {
                            acc.push_str(name);
                            acc.push_str(", ");
                            acc
                        }
                        _ => unreachable!(),
                    }
                });
                let types = format!("({})", types.trim_end_matches(", "));
                ValueNode::Str(types)
            }
            ValueNode::Str(_) => ValueNode::Str(String::from("str")),
            ValueNode::DataStructure { name, .. } => ValueNode::Str(name.to_owned()),
            ValueNode::Reference(v) => v.node().get_type_name(),
            ValueNode::ReferenceMut(v) => v.node().get_type_name(),
            ValueNode::Binding { ty, .. } => ValueNode::Str(ty.0.get_name()),
            ValueNode::Destructure {
                parent,
                accessor,
                ty,
            } => ValueNode::Str(ty.0.get_name()),
            ValueNode::Invoke(invoke_ref) => ValueNode::Str(
                INVOKES
                    .with_borrow(|invokes| invokes[invoke_ref.0].function.sig.output.0.get_name()),
            ),
            node => panic!("ValueNode::get_type_name"),
        }
    }
}
