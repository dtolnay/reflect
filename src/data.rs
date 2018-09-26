use crate::Field;
use crate::Fields;
use crate::Value;

use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub enum Data<T> {
    Struct(Struct<T>),
    Enum(Enum<T>),
}

#[derive(Debug, Clone)]
pub enum Struct<T> {
    Unit(UnitStruct),
    Tuple(TupleStruct<T>),
    Struct(StructStruct<T>),
}

#[derive(Debug, Clone)]
pub struct UnitStruct {
    pub(crate) private: (),
}

#[derive(Debug, Clone)]
pub struct TupleStruct<T> {
    pub(crate) fields: Vec<Field<T>>,
}

#[derive(Debug, Clone)]
pub struct StructStruct<T> {
    pub(crate) fields: Vec<Field<T>>,
}

impl<T> Struct<T> {
    pub fn fields(&self) -> Fields<T>
    where
        T: Clone,
    {
        let fields = match *self {
            Struct::Unit(ref s) => Vec::new(),
            Struct::Tuple(ref s) => s.fields.clone(),
            Struct::Struct(ref s) => s.fields.clone(),
        };
        Fields {
            fields: fields.into_iter(),
        }
    }
}

impl<T> TupleStruct<T> {
    pub fn fields(&self) -> Fields<T>
    where
        T: Clone,
    {
        Fields {
            fields: self.fields.clone().into_iter(),
        }
    }
}

impl<T> StructStruct<T> {
    pub fn fields(&self) -> Fields<T>
    where
        T: Clone,
    {
        Fields {
            fields: self.fields.clone().into_iter(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Enum<T> {
    pub(crate) variants: Vec<Variant<T>>,
}

impl<'a> Enum<Value<'a>> {
    pub fn match_variant<Run>(&self, run: Run) -> Value<'a>
    where
        Run: Fn(Variant<Value<'a>>) -> Value<'a>,
    {
        let mut arms = Vec::new();
        for variant in self.variants.clone() {
            arms.push(run(variant));
        }
        // FIXME introduce a match node
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub enum Variant<T> {
    Unit(UnitVariant),
    Tuple(TupleVariant<T>),
    Struct(StructVariant<T>),
}

#[derive(Debug, Clone)]
pub struct UnitVariant {
    pub(crate) private: (),
}

#[derive(Debug, Clone)]
pub struct TupleVariant<T> {
    pub(crate) phantom: PhantomData<T>,
}

#[derive(Debug, Clone)]
pub struct StructVariant<T> {
    pub(crate) phantom: PhantomData<T>,
}
