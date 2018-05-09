#![recursion_limit = "256"]

#[macro_use]
extern crate quote;

#[macro_use]
extern crate reflect;

extern crate proc_macro2;

mod debug;

use proc_macro2::TokenStream;

#[test]
fn test_debug() {
    let input = TokenStream::from(quote! {
        struct Point {
            x: i32,
            y: i32,
        }
    });

    let expected = TokenStream::from(quote! {
        impl ::std::fmt::Debug for Point {
            fn fmt(&self, __arg0: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                /*
                // TODO: somewhat more intelligent impl
                match *self {
                    Point { x: ref __v3, y: ref __v4 } => {
                        let mut __v5 = ::std::fmt::Formatter::debug_struct(__arg0, "Point");
                        let _ = ::std::fmt::DebugStruct::field(&mut __v5, "x", __v3);
                        let _ = ::std::fmt::DebugStruct::field(&mut __v5, "y", __v4);
                        let __v11 = ::std::fmt::DebugStruct::finish(&mut __v5);
                        __v11
                    }
                }
                */

                let __v0 = self;
                let __v1 = __arg0;
                let __v2 = "Point";
                let __v3 = &__v0.x;
                let __v4 = &__v0.y;
                let mut __v5 = ::std::fmt::Formatter::debug_struct(__v1, __v2);
                let __v6 = &mut __v5;
                let __v7 = "x";
                let _ = ::std::fmt::DebugStruct::field(__v6, __v7, __v3);
                let __v9 = "y";
                let _ = ::std::fmt::DebugStruct::field(__v6, __v9, __v4);
                let __v11 = ::std::fmt::DebugStruct::finish(__v6);
                __v11
            }
        }
    });

    let actual = reflect::derive(input, debug::derive);
    assert_eq!(actual.to_string(), expected.to_string());
}
