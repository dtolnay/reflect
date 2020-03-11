use crate::{attr, Field, Fields, Value};
use std::fmt;
use std::fmt::Debug;
use std::marker::PhantomData;
use syn::Attribute;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Data<T> {
    Struct(Struct<T>),
    Enum(Enum<T>),
}

impl<T> Data<T> {
    pub fn attrs(&self) -> &[Attribute] {
        match self {
            Data::Struct(s) => &s.attrs(),
            Data::Enum(e) => &e.attrs,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Struct<T> {
    Unit(UnitStruct),
    Tuple(TupleStruct<T>),
    Struct(StructStruct<T>),
}

impl<T> Struct<T> {
    pub fn attrs(&self) -> &[Attribute] {
        match self {
            Struct::Unit(us) => &us.attrs,
            Struct::Tuple(ts) => &ts.attrs,
            Struct::Struct(ss) => &ss.attrs,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnitStruct {
    pub(crate) attrs: Vec<Attribute>,
}

impl Debug for UnitStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("UnitStruct")
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TupleStruct<T> {
    pub(crate) fields: Vec<Field<T>>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for TupleStruct<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TupleStruct")
            .field("fields", &self.fields)
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructStruct<T> {
    pub(crate) fields: Vec<Field<T>>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for StructStruct<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("StructStruct")
            .field("fields", &self.fields)
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

impl<T> Struct<T> {
    pub fn fields(&self) -> Fields<T>
    where
        T: Clone,
    {
        let fields = match self {
            Struct::Unit(s) => Vec::new(),
            Struct::Tuple(s) => s.fields.clone(),
            Struct::Struct(s) => s.fields.clone(),
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

    pub fn attrs(&self) -> &[Attribute] {
        &self.attrs
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

    pub fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Enum<T> {
    pub(crate) variants: Vec<Variant<T>>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for Enum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Enum")
            .field("variants", &self.variants)
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

impl Enum<Value> {
    pub fn match_variant<Run>(&self, run: Run) -> Value
    where
        Run: Fn(Variant<Value>) -> Value,
    {
        let mut arms = Vec::new();
        for variant in self.variants.clone() {
            arms.push(run(variant));
        }
        // FIXME introduce a match node
        unimplemented!()
    }

    pub fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variant<T> {
    Unit(UnitVariant),
    Tuple(TupleVariant<T>),
    Struct(StructVariant<T>),
}

impl<T> Variant<T> {
    pub fn attrs(&self) -> &[Attribute] {
        match self {
            Variant::Unit(uv) => &uv.attrs,
            Variant::Tuple(tv) => &tv.attrs,
            Variant::Struct(sv) => &sv.attrs,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnitVariant {
    pub(crate) attrs: Vec<Attribute>,
}

impl Debug for UnitVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("UnitVariant")
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TupleVariant<T> {
    pub(crate) phantom: PhantomData<T>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for TupleVariant<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TupleVariant")
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructVariant<T> {
    pub(crate) phantom: PhantomData<T>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for StructVariant<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("StructVariant")
            .field("attrs", attr::debug(&self.attrs))
            .finish()
    }
}
