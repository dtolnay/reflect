use quote::quote;
use reflect::*;

library! {
    use std {
        mod string {
            type String;
        }
    }
    use simple {
        type Wrapper<T>;

        trait Simple<T> {
            fn simple<U>(T, Wrapper<U>, Wrapper<Wrapper<::std::string::String>>);
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
        struct Generics<'a, 'b, T: ::simple::Bound + Send> where 'b: 'static {
            pub param: &'a &'static T
        }
    };

    let expected = quote! {
        impl<'__a1, '__a2, __T0, __T1>  ::simple::Simple<__T1> for Generics<'__a1, '__a2, __T0>
            where
            __T0: ::simple::Bound + Send,
            '__a2: 'static,
        {
            fn simple<__T2>(__arg0: __T1, __arg1: ::simple::Wrapper<__T2>, __arg2: ::simple::Wrapper<::simple::Wrapper<::std::string::String> >) {
                let __v0 = ();
                __v0
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
