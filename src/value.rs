use crate::{Accessor, Data, GlobalBorrow, Type, TypeNode, ValueNode, ValueRef, GLOBAL_DATA};

#[derive(Debug, Clone, Copy)]
pub struct Value {
    pub(crate) index: ValueRef,
}

impl Value {
    pub fn tuple(values: &[Self]) -> Self {
        let node = ValueNode::Tuple(values.iter().map(|v| v.index).collect());
        Value {
            index: node.index_push(),
        }
    }

    pub fn reference(&self) -> Self {
        let node = ValueNode::Reference(self.index);
        Value {
            index: node.index_push(),
        }
    }

    pub fn reference_mut(&self) -> Self {
        let node = ValueNode::ReferenceMut(self.index);
        Value {
            index: node.index_push(),
        }
    }

    pub fn dereference(&self) -> Self {
        match self.node() {
            ValueNode::Reference(inner) => Value { index: inner },
            ValueNode::ReferenceMut(inner) => Value { index: inner },
            other => {
                let node = ValueNode::Dereference(self.index);
                Value {
                    index: node.index_push(),
                }
            }
        }
    }

    pub fn get_type_name(&self) -> Self {
        let node = self.node().get_type_name();
        Value {
            index: node.index_push(),
        }
    }

    pub fn data(&self) -> Data<Self> {
        use crate::ValueNode::*;
        match self.node() {
            DataStructure { data, .. } => data.clone().map(|value_ref| Value {
                index: value_ref.element,
            }),
            Reference(v) => Value { index: v }.data().map(|v| v.element.reference()),
            ReferenceMut(v) => Value { index: v }.data().map(|v| v.element.reference_mut()),
            // FIXME generate match and propagate the binding
            Binding { name, ty } => ty.data().map(|field| {
                let node = ValueNode::Destructure {
                    parent: self.index,
                    accessor: field.accessor.clone(),
                    ty: field.element,
                };
                Value {
                    index: node.index_push(),
                }
            }),
            _ => panic!("Value::data"),
        }
    }

    /// Returns a Value from a Tuple
    pub fn get_tuple_value(&self, index: usize) -> Self {
        match self.index.node() {
            ValueNode::Tuple(values) => Value {
                index: values[index],
            },
            ValueNode::Binding {
                ty: Type(TypeNode::Tuple(types)),
                ..
            } => {
                if index >= types.len() {
                    panic!("Value:get_tuple_value: Out of bounds")
                }
                let node = ValueNode::Destructure {
                    parent: self.index,
                    accessor: Accessor::Index(index),
                    ty: types[index].clone(),
                };
                Value {
                    index: node.index_push(),
                }
            }
            _ => panic!("Value:get_tuple_value: Not a Tuple"),
        }
    }
}

impl Value {
    pub(crate) fn node(self) -> ValueNode {
        self.index.node()
    }
}

impl ValueRef {
    pub(crate) fn node(self) -> ValueNode {
        GLOBAL_DATA.with_borrow(|global| global.node(self))
    }
}
