use Data;
use Enum;
use Field;
use Struct;
use StructStruct;
use StructVariant;
use TupleStruct;
use TupleVariant;
use Variant;

use std::marker::PhantomData;

impl<T> Data<T> {
    pub fn map<F, R>(self, f: F) -> Data<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        match self {
            Data::Struct(data) => Data::Struct(data.map(f)),
            Data::Enum(data) => Data::Enum(data.map(f)),
        }
    }
}

impl<T> Struct<T> {
    pub fn map<F, R>(self, f: F) -> Struct<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        match self {
            Struct::Unit(s) => Struct::Unit(s),
            Struct::Tuple(s) => Struct::Tuple(s.map(f)),
            Struct::Struct(s) => Struct::Struct(s.map(f)),
        }
    }
}

impl<T> TupleStruct<T> {
    pub fn map<F, R>(self, mut f: F) -> TupleStruct<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        TupleStruct {
            fields: self.fields
                .into_iter()
                .map(|field| field.map(&mut f))
                .collect(),
        }
    }
}

impl<T> StructStruct<T> {
    pub fn map<F, R>(self, mut f: F) -> StructStruct<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        StructStruct {
            fields: self.fields
                .into_iter()
                .map(|field| field.map(&mut f))
                .collect(),
        }
    }
}

impl<T> Field<T> {
    pub fn map<F, R>(self, f: F) -> Field<R>
    where
        F: FnOnce(Field<T>) -> R,
    {
        let name = self.name.clone();
        Field {
            name,
            element: f(self),
        }
    }
}

impl<T> Enum<T> {
    pub fn map<F, R>(self, mut f: F) -> Enum<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        Enum {
            variants: self.variants.into_iter().map(|v| v.map(&mut f)).collect(),
        }
    }
}

impl<T> Variant<T> {
    pub fn map<F, R>(self, f: F) -> Variant<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        match self {
            Variant::Unit(v) => Variant::Unit(v),
            Variant::Tuple(v) => Variant::Tuple(v.map(f)),
            Variant::Struct(v) => Variant::Struct(v.map(f)),
        }
    }
}

impl<T> TupleVariant<T> {
    pub fn map<F, R>(self, f: F) -> TupleVariant<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        TupleVariant {
            phantom: PhantomData,
        }
    }
}

impl<T> StructVariant<T> {
    pub fn map<F, R>(self, f: F) -> StructVariant<R>
    where
        F: FnMut(Field<T>) -> R,
    {
        StructVariant {
            phantom: PhantomData,
        }
    }
}
