use quote::quote;

mod display;

/// This test mimics the [displaydoc] basic crate functionality.
///
/// [displaydoc]: https://github.com/yaahc/displaydoc
#[test]
fn test_displaydoc() {
    let input = quote! {
        /// Point in space:
        struct Point {
            /// x: {},
            x: i32,

            /// y: {}
            y: i32,
        }
    };

    let expected = quote! {
        impl ::std::fmt::Display for Point {
            fn fmt(&self, __arg0: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let __v0 = self;
                let __v1 = __arg0;
                let __v3 = &__v0.x;
                let __v4 = &__v0.y;
                let _ = write!(__v1, "Point in space: ");
                let _ = write!(__v1, "x: {}, ", __v3);
                let __v10 = write!(__v1, "y: {}", __v4);
                __v10
            }
        }
    };

    let actual = reflect::derive(input, display::derive);
    assert_eq!(actual.to_string(), expected.to_string());
}
