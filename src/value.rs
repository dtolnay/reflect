use crate::Data;
use crate::Ident;
use crate::Push;
use crate::StaticBorrow;
use crate::ValueNode;
use crate::ValueRef;
use crate::WIP;

#[derive(Debug, Clone, Copy)]
pub struct Value {
    pub(crate) index: ValueRef,
}

impl Value {
    pub fn reference(&self) -> Self {
        let node = ValueNode::Reference(self.index);
        Value {
            index: WIP.with_borrow_mut(|wip| wip.values.index_push(node)),
        }
    }

    pub fn reference_mut(&self) -> Self {
        let node = ValueNode::ReferenceMut(self.index);
        Value {
            index: WIP.with_borrow_mut(|wip| wip.values.index_push(node)),
        }
    }

    pub fn dereference(&self) -> Self {
        match self.node() {
            ValueNode::Reference(inner) => Value { index: inner },
            ValueNode::ReferenceMut(inner) => Value { index: inner },
            ref other => {
                let node = ValueNode::Dereference(self.index);
                Value {
                    index: WIP.with_borrow_mut(|wip| wip.values.index_push(node)),
                }
            }
        }
    }

    pub fn get_type_name(&self) -> Self {
        let node = self.node().get_type_name();
        Value {
            index: WIP.with_borrow_mut(|wip| wip.values.index_push(node)),
        }
    }

    pub fn data(&self) -> Data<Self> {
        use crate::ValueNode::*;
        match self.node() {
            DataStructure { ref data, .. } => data.clone().map(|value_ref| Value {
                index: value_ref.element,
            }),
            Reference(v) => Value { index: v }.data().map(|v| v.element.reference()),
            ReferenceMut(v) => Value { index: v }.data().map(|v| v.element.reference_mut()),
            // FIXME generate match and propagate the binding
            Binding { ref name, ref ty } => ty.data().map(|ty| {
                let node = ValueNode::Destructure {
                    parent: self.index,
                    // FIXME does not work for tuple struct fields
                    field: Ident::new(ty.name.clone()),
                    ty: ty.element.clone(),
                };
                Value {
                    index: WIP.with_borrow_mut(|wip| wip.values.index_push(node)),
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
        WIP.with_borrow(|wip| wip.node(self))
    }
}
