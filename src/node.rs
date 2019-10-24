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
    Binding { name: Ident, ty: Type },
    DataStructure { name: String, data: Data<ValueRef> },
    Invoke(InvokeRef),
    Destructure { parent: ValueRef, field: Ident },
}

impl ValueNode {
    pub fn get_type_name(&self) -> Self {
        match self {
            ValueNode::Tuple(ref types) => {
                let types: String = types.iter().fold(String::from(""), |mut acc, v| {
                    match v.node().get_type_name() {
                        ValueNode::Str(name) => {
                            acc.push_str(&name);
                            acc.push_str(", ");
                            acc
                        }
                        _ => unreachable!(),
                    }
                });
                let types = format!("({})", types.trim_end_matches(", "));
                ValueNode::Str(types)
            }
            ValueNode::DataStructure { ref name, .. } => ValueNode::Str(name.to_owned()),
            ValueNode::Reference(v) => v.node().get_type_name().map_str(|s| format!("&{}", s)),
            ValueNode::ReferenceMut(v) => {
                v.node().get_type_name().map_str(|s| format!("&mut {}", s))
            }
            ValueNode::Binding { ref ty, .. } => ValueNode::Str(ty.0.get_name()),
            _ => panic!("Value::get_type_name"),
        }
    }

    fn map_str<F>(self, f: F) -> Self
    where
        F: FnOnce(String) -> String,
    {
        match self {
            ValueNode::Str(s) => ValueNode::Str(f(s)),
            other => other,
        }
    }
}
