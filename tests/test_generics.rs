#![allow(non_upper_case_globals)] // rustc bug: https://github.com/rust-lang/rust/issues/110573
#![allow(clippy::wildcard_imports)]

use quote::quote;
use reflect::*;

library! {
    use simple {
        trait Simple {
            fn simple();
        }

        trait Bound {}
    }
}

fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::simple::Simple, ex.target_type(), |block| {
        block.make_function(RUNTIME::simple::Simple::simple, |make_function| {
            make_function.unit()
        });
    });
}

#[test]
fn test_generics() {
    let input = quote! {
        struct Generics<'a, 'b,  T: ::simple::Bound + Send> where 'b: 'a {
            pub param: &'a &'b T
        }
    };

    let expected = quote! {
        impl<'a, 'b, T>  ::simple::Simple for Generics<'a, 'b, T>
            where
                T: ::simple::Bound + Send,
                'b: 'a,
        {
            fn simple() {
                let __v0 = ();
                __v0
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
