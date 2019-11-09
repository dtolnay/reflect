#![recursion_limit = "256"]

use quote::quote;

mod debug;

#[test]
fn test_debug() {
    let input = quote! {
        struct Point {
            x: i32,
            y: i32,
        }
    };

    let expected = quote! {
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
                let __v3 = &__v0.x;
                let __v4 = &__v0.y;
                let mut __v5 = ::std::fmt::Formatter::debug_struct(__v1, "Point");
                let __v6 = &mut __v5;
                let _ = ::std::fmt::DebugStruct::field(__v6, "x", __v3);
                let _ = ::std::fmt::DebugStruct::field(__v6, "y", __v4);
                let __v11 = ::std::fmt::DebugStruct::finish(__v6);
                __v11
            }
        }
    };

    let actual = reflect::derive(input, debug::derive);
    assert_eq!(actual.to_string(), expected.to_string());
}
