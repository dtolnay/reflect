use quote::quote;
use reflect::*;

library! {
    use std {
        mod string {
            type String;
        }
    }
    use generic {
        type Wrapper<T>;

        trait Generic<T> {
            fn generic<U>(self, T, Wrapper<U>, Wrapper<Wrapper<::std::string::String>>);
        }

        trait Bound {}
    }
}

#[test]
fn test_generics1() {
    let input = quote! {
        struct Generics<'a, 'b, T: ::generic::Bound + Send> where 'b: 'static {
            pub param: &'a &'static T
        }
    };

    fn derive(ex: Execution) {
        ex.make_trait_impl(RUNTIME::generic::Generic, ex.target_type(), |block| {
            block.make_function(RUNTIME::generic::Generic::generic, |make_function| {
                make_function.unit()
            });
        });
    }

    let expected = quote! {
        impl<'__a1, __T0, __T1> ::generic::Generic<__T1> for Generics<'__a1, 'static, __T0>
        where
            __T0: ::generic::Bound + Send,
        {
            fn generic<__T2>(
                self,
                __arg0: __T1,
                __arg1: ::generic::Wrapper<__T2>,
                __arg2: ::generic::Wrapper<::generic::Wrapper<::std::string::String> >
            ) {
                let __v0 = ();
                __v0
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}

#[test]
fn test_generics2() {
    let input = quote! {
        struct GenericQuadruple<S, T, U, V> {
            one: S,
            two: T,
            three: U,
            four: Wrapper<V>,
        }
    };

    fn derive(ex: Execution) {
        ex.make_trait_impl(RUNTIME::generic::Generic, ex.target_type(), |block| {
            block.make_function(RUNTIME::generic::Generic::generic, |make_function| {
                let reciever = make_function.arg(0);
                match reciever.data() {
                    Data::Struct(Struct::Struct(receiver)) => {
                        let mut fields = receiver.fields();
                        let one = fields.next().unwrap().get_value();
                        let two = fields.next().unwrap().get_value();
                        let three = fields.next().unwrap().get_value();
                        let four = fields.next().unwrap().get_value();
                        RUNTIME::generic::Generic::generic.INVOKE(one, two, three, four);
                        make_function.unit()
                    }
                    _ => unimplemented!(),
                }
            });
        });
    }

    let expected = quote! {
        impl<__T0, __T1, __T4, __T7> ::generic::Generic<__T4>
            for GenericQuadruple<
                __T0,
                __T1,
                ::generic::Wrapper<__T7>,
                ::generic::Wrapper<::std::string::String>
            >
        where
            __T0: ::generic::Generic<__T1>,
        {
            fn generic<__T5>(
                self,
                __arg0: __T4,
                __arg1: ::generic::Wrapper<__T5>,
                __arg2: ::generic::Wrapper<::generic::Wrapper<::std::string::String> >
            )
            {
                let __v0 = self;
                let __v1 = __v0.one;
                let __v2 = __v0.two;
                let __v3 = __v0.three;
                let __v4 = __v0.four;
                let _ = ::generic::Generic::generic(__v1, __v2, __v3, __v4);
                let __v6 = ();
                __v6
            }
        }
    };

    let expected_start = quote! {
        impl<__T0, __T1, __T4, __T7> ::generic::Generic<__T4>
            for GenericQuadruple<
                __T0,
                __T1,
                ::generic::Wrapper<__T7>,
                ::generic::Wrapper<::std::string::String>
            >
        where
            __T0: ::generic::Generic<__T1>,
    };

    let expected_generic = quote! {
        fn generic<__T5>(
            self,
            __arg0: __T4,
            __arg1: ::generic::Wrapper<__T5>,
            __arg2: ::generic::Wrapper<::generic::Wrapper<::std::string::String> >
        )
    };

    let output = reflect::derive(input, derive);
    let output = output.to_string();

    // The code gen produces wrong output
    assert_ne!(&output, &expected.to_string());

    // However the trait and type inference is correct
    assert!(output.starts_with(&expected_start.to_string()));
    assert!(output.contains(&expected_generic.to_string()));
}
