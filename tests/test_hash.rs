use quote::quote;
use reflect::*;

library! {
    extern crate std {
        mod hash {
            trait Hasher {}

            trait Hash {
                fn hash<H: Hasher>(&self, &mut H);
            }
        }
    }
}

fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::std::hash::Hash, ex.target_type(), |block| {
        block.make_function(RUNTIME::std::hash::Hash::hash, make_hash);
    });
}

fn make_hash(f: MakeFunction) -> Value {
    let receiver = f.arg(0);
    let hasher = f.arg(1);

    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(_receiver) => unimplemented!(),
            Struct::Tuple(_receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                for field in receiver.fields() {
                    RUNTIME::std::hash::Hash::hash.INVOKE(field.get_value(), hasher);
                }
                f.unit()
            }
        },
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(_variant) => unimplemented!(),
            Variant::Tuple(_variant) => unimplemented!(),
            Variant::Struct(_variant) => unimplemented!(),
        }),
    }
}

#[test]
fn test_hash() {
    let input = quote! {
        struct Point {
            x: i32,
            y: i32,
        }
    };

    let expected = quote! {
        impl ::std::hash::Hash for Point {
            fn hash<'__a1, '__a2, __T0>(&'__a1 self, __arg0: &'__a2 mut __T0)
            where
                __T0: ::std::hash::Hasher,
            {
                let __v0 = self;
                let __v1 = __arg0;
                let __v2 = &__v0.x;
                let __v3 = &__v0.y;
                let _ = ::std::hash::Hash::hash(__v2, __v1);
                let _ = ::std::hash::Hash::hash(__v3, __v1);
                let __v6 = ();
                __v6
            }
        }
    };

    let actual = reflect::derive(input, derive);
    assert_eq!(actual.to_string(), expected.to_string());
}

#[test]
fn test_generic_hash() {
    let input = quote! {
        struct Generic<T, U> {
            name: String,
            one: T,
            two: U,
        }
    };

    let expected = quote! {
        impl<__T0, __T1> ::std::hash::Hash for Generic<__T0, __T1>
        where
            __T0: ::std::hash::Hash,
            __T1: ::std::hash::Hash,
        {
            fn hash<'__a1, '__a2, __T2>(&'__a1 self, __arg0: &'__a2 mut __T2)
            where
                __T2: ::std::hash::Hasher,
            {
                let __v0 = self;
                let __v1 = __arg0;
                let __v2 = &__v0.name;
                let __v3 = &__v0.one;
                let __v4 = &__v0.two;
                let _ = ::std::hash::Hash::hash(__v2, __v1);
                let _ = ::std::hash::Hash::hash(__v3, __v1);
                let _ = ::std::hash::Hash::hash(__v4, __v1);
                let __v8 = ();
                __v8
            }
        }
    };

    let actual = reflect::derive(input, derive);
    assert_eq!(actual.to_string(), expected.to_string());
}
