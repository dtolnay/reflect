#![recursion_limit = "256"]

use quote::quote;

mod display;

#[test]
fn test_displaydoc() {
    let input = quote! {
        struct Point {
            /// x-coordinate: {x}
            x: i32,

            /// y-coordinate: {y}
            y: i32,
        }
    };

    let expected = quote! {
        impl ::core::fmt::Display for Point {
            fn fmt(&self, __arg0: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {

            }
        }
    };

    let actual = reflect::derive(input, display::derive);
    println!("{}", actual.to_string());
    assert_eq!(actual.to_string(), expected.to_string());
}
