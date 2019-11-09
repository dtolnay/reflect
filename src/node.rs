use crate::Accessor;
use crate::Data;
use crate::Ident;
use crate::InvokeRef;
use crate::Type;
use crate::ValueRef;

#[derive(Debug, Clone)]
pub(crate) enum ValueNode {
    Tuple(Vec<ValueRef>),
    Str(String),
    Reference(ValueRef),
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
    MacroInvocation(InvokeRef),
}

impl ValueNode {
    pub fn get_type_name(&self) -> Self {
        match self {
            ValueNode::Tuple(ref types) => {
                let types: String = types.iter().fold(String::from(""), |mut acc, v| {
                    match v.node().get_type_name() {
                        ValueNode::Str(ref name) => {
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
            ValueNode::DataStructure { ref name, .. } => ValueNode::Str(name.to_owned()),
            ValueNode::Reference(v) => v.node().get_type_name(),
            ValueNode::ReferenceMut(v) => v.node().get_type_name(),
            ValueNode::Binding { ref ty, .. } => ValueNode::Str(ty.0.get_name()),
            ValueNode::Destructure {
                parent,
                accessor,
                ty,
            } => ValueNode::Str(ty.0.get_name()),
            node => panic!("ValueNode::get_type_name"),
        }
    }
}
