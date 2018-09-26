use crate::Push;
use crate::Type;
use crate::Value;
use crate::ValueNode;

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
pub struct Field<T> {
    pub(crate) name: String,
    pub(crate) element: T,
}

impl<'a> Field<Value<'a>> {
    pub fn get_name(&self) -> Value<'a> {
        let node = ValueNode::Str(self.name.clone());
        self.element
            .relative(self.element.function.values.borrow_mut().index_push(node))
    }
}

impl Field<Type> {
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

impl<'a> Field<Value<'a>> {
    pub fn get_value(&self) -> Value<'a> {
        self.element
    }
}
