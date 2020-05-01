use crate::{
    Accessor, Data, GlobalBorrow, GlobalPush, Type, TypeNode, ValueNode, ValueRef, VALUES,
};

#[derive(Debug, Clone, Copy)]
pub struct Value {
    pub(crate) index: ValueRef,
}

impl Value {
    pub fn tuple(values: &[Self]) -> Self {
        let node = ValueNode::Tuple(values.iter().map(|v| v.index).collect());
        Value {
            index: VALUES.index_push(node),
        }
    }

    pub fn reference(&self) -> Self {
        let node = ValueNode::Reference {
            is_mut: false,
            value: self.index,
        };
        Value {
            index: VALUES.index_push(node),
        }
    }

    pub fn reference_mut(&self) -> Self {
        let node = ValueNode::Reference {
            is_mut: true,
            value: self.index,
        };
        Value {
            index: VALUES.index_push(node),
        }
    }

    pub fn dereference(&self) -> Self {
        match self.node() {
            ValueNode::Reference { value, .. } => Value { index: value },
            other => {
                let node = ValueNode::Dereference(self.index);
                Value {
                    index: VALUES.index_push(node),
                }
            }
        }
    }

    pub fn get_type_name(&self) -> Self {
        let node = self.node().get_type_name();
        Value {
            index: VALUES.index_push(node),
        }
    }

    pub fn data(&self) -> Data<Self> {
        use crate::ValueNode::*;
        match self.node() {
            DataStructure { data, .. } => data.map(|value_ref| Value {
                index: value_ref.element,
            }),
            Reference { is_mut, value } if !is_mut => {
                Value { index: value }.data().map(|v| v.element.reference())
            }

            Reference { is_mut, value } if is_mut => Value { index: value }
                .data()
                .map(|v| v.element.reference_mut()),

            // FIXME generate match and propagate the binding
            Binding { name, ty } => ty.data().map(|field| {
                let node = ValueNode::Destructure {
                    parent: self.index,
                    accessor: field.accessor.clone(),
                    ty: field.element,
                };
                Value {
                    index: VALUES.index_push(node),
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
                    ty: Type(types[index].clone()),
                };
                Value {
                    index: VALUES.index_push(node),
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
        VALUES.with_borrow(|values| values[self.0].clone())
    }
}
