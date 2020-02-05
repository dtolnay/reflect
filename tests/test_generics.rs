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
        struct Generics<'a,  T: ::simple::Bound + Send> where 'b: 'static {
            pub param: &'a &'static T
        }
    };

    let expected = quote! {
        impl<'a, T>  ::simple::Simple<T> for Generics<'a, T>
            where
                T: ::simple::Bound + Send,
                'b: 'static,
        {
            fn simple(__arg0: T, __arg1: ::simple::Wrapper<U>, __arg2: ::simple::Wrapper<::simple::Wrapper<::std::string::String> >) {
                let __v0 = ();
                __v0
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
