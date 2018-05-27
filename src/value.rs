use Data;
use Ident;
use Push;
use ValueNode;
use ValueRef;
use WipFunction;

#[derive(Debug, Clone, Copy)]
pub struct Value<'a> {
    pub(crate) function: &'a WipFunction,
    pub(crate) index: ValueRef,
}

impl<'a> Value<'a> {
    pub fn reference(&self) -> Self {
        let node = ValueNode::Reference(self.index);
        self.relative(self.function.values.borrow_mut().index_push(node))
    }

    pub fn reference_mut(&self) -> Self {
        let node = ValueNode::ReferenceMut(self.index);
        self.relative(self.function.values.borrow_mut().index_push(node))
    }

    pub fn dereference(&self) -> Self {
        match self.node() {
            ValueNode::Reference(inner) => self.relative(inner),
            ValueNode::ReferenceMut(inner) => self.relative(inner),
            ref other => {
                let node = ValueNode::Dereference(self.index);
                self.relative(self.function.values.borrow_mut().index_push(node))
            }
        }
    }

    pub fn get_type_name(&self) -> Self {
        match self.node() {
            ValueNode::DataStructure { ref name, .. } => {
                let node = ValueNode::Str(name.to_owned());
                self.relative(self.function.values.borrow_mut().index_push(node))
            }
            ValueNode::Reference(v) => self.relative(v).get_type_name(),
            ValueNode::ReferenceMut(v) => self.relative(v).get_type_name(),
            ValueNode::Binding { ref ty, .. } => {
                let node = ValueNode::Str(ty.0.get_name());
                self.relative(self.function.values.borrow_mut().index_push(node))
            }
            _ => panic!("Value::get_type_name"),
        }
    }

    pub fn data(&self) -> Data<Self> {
        use ValueNode::*;
        match self.node() {
            DataStructure { ref data, .. } => data
                .clone()
                .map(|value_ref| self.relative(value_ref.element)),
            Reference(v) => self.relative(v).data().map(|v| v.element.reference()),
            ReferenceMut(v) => self.relative(v).data().map(|v| v.element.reference_mut()),
            // FIXME generate match and propagate the binding
            Binding { ref name, ref ty } => ty.data().map(|ty| {
                let node = ValueNode::Destructure {
                    parent: self.index,
                    // FIXME does not work for tuple struct fields
                    field: Ident::new(ty.name),
                };
                Value {
                    function: self.function,
                    index: self.function.values.borrow_mut().index_push(node),
                }
            }),
            _ => panic!("Value::data"),
        }
    }
}

impl<'a> Value<'a> {
    pub(crate) fn node(self) -> ValueNode {
        self.function.node(self.index)
    }

    pub(crate) fn relative(self, index: ValueRef) -> Self {
        Value {
            function: self.function,
            index,
        }
    }
}
