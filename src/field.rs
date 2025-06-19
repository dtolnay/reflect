use crate::{attr, Ident, Push, StaticBorrow, Type, Value, ValueNode, WIP};
use std::fmt::{self, Debug, Display};
use std::vec;
use syn::Attribute;

#[derive(Debug, Clone)]
pub struct Fields<T> {
    pub(crate) fields: vec::IntoIter<Field<T>>,
}

impl<T> Iterator for Fields<T> {
    type Item = Field<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.fields.next()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Accessor {
    Name(Ident),
    Index(usize),
}

#[derive(Clone)]
pub struct Field<T> {
    pub(crate) accessor: Accessor,
    pub(crate) element: T,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for Field<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Field")
            .field("accessor", &self.accessor)
            .field("element", &self.element)
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

impl Display for Accessor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Accessor::*;
        match self {
            Name(ident) => Display::fmt(ident, f),
            Index(i) => Display::fmt(i, f),
        }
    }
}

impl Field<Value> {
    pub fn get_name(&self) -> Value {
        let node = ValueNode::Str(self.accessor.to_string());
        Value {
            index: WIP.with_borrow_mut(|wip| wip.values.index_push(node)),
        }
    }

    pub fn get_attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl Field<Type> {
    pub fn get_name(&self) -> String {
        self.accessor.to_string()
    }

    pub fn get_attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl Field<Value> {
    pub fn get_value(&self) -> Value {
        self.element
    }
}

impl Field<Type> {
    pub fn get_type(&self) -> &Type {
        &self.element
    }
}
