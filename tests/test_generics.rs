use quote::quote;
use reflect::*;

library! {
    use simple {
        type Wrapper<T>;

        trait Simple<T> {
            fn simple<U>(T, Wrapper<U>);
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
            fn simple(__arg0: T, __arg1: ::simple::Wrapper<U>) {
                let __v0 = ();
                __v0
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
