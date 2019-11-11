use crate::{Field, Fields, Value};
use quote::ToTokens;
use std::fmt;
use std::fmt::Debug;
use std::marker::PhantomData;
use syn::Attribute;

#[derive(Debug, Clone)]
pub enum Data<T> {
    Struct(Struct<T>),
    Enum(Enum<T>),
}

impl<T> Data<T> {
    pub fn attrs(&self) -> &[Attribute] {
        match &self {
            Data::Struct(s) => &s.attrs(),
            Data::Enum(e) => &e.attrs,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Struct<T> {
    Unit(UnitStruct),
    Tuple(TupleStruct<T>),
    Struct(StructStruct<T>),
}

impl<T> Struct<T> {
    pub fn attrs(&self) -> &[Attribute] {
        match &self {
            Struct::Unit(us) => &us.attrs,
            Struct::Tuple(ts) => &ts.attrs,
            Struct::Struct(ss) => &ss.attrs,
        }
    }
}

#[derive(Clone)]
pub struct UnitStruct {
    pub(crate) attrs: Vec<Attribute>,
}

impl Debug for UnitStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct UnitStructDebug {
            attrs: Vec<String>,
        }

        let view = UnitStructDebug {
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
    }
}

#[derive(Clone)]
pub struct TupleStruct<T> {
    pub(crate) fields: Vec<Field<T>>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for TupleStruct<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct TupleStructDebug<'a, T> {
            fields: &'a Vec<Field<T>>,
            attrs: Vec<String>,
        }

        let view = TupleStructDebug {
            fields: &self.fields,
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
    }
}

#[derive(Clone)]
pub struct StructStruct<T> {
    pub(crate) fields: Vec<Field<T>>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for StructStruct<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct StructStructDebug<'a, T> {
            fields: &'a Vec<Field<T>>,
            attrs: Vec<String>,
        }

        let view = StructStructDebug {
            fields: &self.fields,
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
    }
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

#[derive(Clone)]
pub struct Enum<T> {
    pub(crate) variants: Vec<Variant<T>>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for Enum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct EnumDebug<'a, T> {
            variants: &'a Vec<Variant<T>>,
            attrs: Vec<String>,
        }

        let view = EnumDebug {
            variants: &self.variants,
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
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

#[derive(Debug, Clone)]
pub enum Variant<T> {
    Unit(UnitVariant),
    Tuple(TupleVariant<T>),
    Struct(StructVariant<T>),
}

impl<T> Variant<T> {
    pub fn attrs(&self) -> &[Attribute] {
        match &self {
            Variant::Unit(uv) => &uv.attrs,
            Variant::Tuple(tv) => &tv.attrs,
            Variant::Struct(sv) => &sv.attrs,
        }
    }
}

#[derive(Clone)]
pub struct UnitVariant {
    pub(crate) attrs: Vec<Attribute>,
}

impl Debug for UnitVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct UnitVariantDebug {
            attrs: Vec<String>,
        }

        let view = UnitVariantDebug {
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
    }
}

#[derive(Clone)]
pub struct TupleVariant<T> {
    pub(crate) phantom: PhantomData<T>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for TupleVariant<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct TupleVariantDebug<T> {
            phantom: PhantomData<T>,
            attrs: Vec<String>,
        }

        let view = TupleVariantDebug {
            phantom: PhantomData::<T>,
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
    }
}

#[derive(Clone)]
pub struct StructVariant<T> {
    pub(crate) phantom: PhantomData<T>,
    pub(crate) attrs: Vec<Attribute>,
}

impl<T: Debug> Debug for StructVariant<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[derive(Debug)]
        struct StructVariantDebug<T> {
            phantom: PhantomData<T>,
            attrs: Vec<String>,
        }

        let view = StructVariantDebug {
            phantom: PhantomData::<T>,
            attrs: self
                .attrs
                .iter()
                .map(|a| a.to_token_stream().to_string())
                .collect(),
        };

        Debug::fmt(&view, f)
    }
}
