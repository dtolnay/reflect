use crate::Ident;
use crate::Push;
use crate::StaticBorrow;
use crate::Type;
use crate::Value;
use crate::ValueNode;
use crate::WIP;

use std::fmt::{self, Display};
use std::vec;

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

#[derive(Debug, Clone)]
pub struct Field<T> {
    pub(crate) accessor: Accessor,
    pub(crate) element: T,
}

impl Display for Accessor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Accessor::*;
        match self {
            Name(ident) => ident.fmt(f),
            Index(i) => i.fmt(f),
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
}

impl Field<Type> {
    pub fn get_name(&self) -> String {
        self.accessor.to_string()
    }
}

impl Field<Value> {
    pub fn get_value(&self) -> Value {
        self.element
    }
}
