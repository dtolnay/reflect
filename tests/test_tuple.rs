use quote::quote;
use reflect::*;

library! {
    use tuple {
        trait Tuple<T, U> {
            fn swap((T, U)) -> (U, T);
            fn to_tuple(self) -> (T, U);
        }
    }
}

fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::tuple::Tuple, ex.target_type(), |block| {
        block.make_function(RUNTIME::tuple::Tuple::swap, |make_function| {
            let tuple = make_function.arg(0);
            let (t, u) = (tuple.get_index(0), tuple.get_index(1));
            Value::tuple(&[u, t])
        });

        block.make_function(RUNTIME::tuple::Tuple::to_tuple, |make_function| {
            let receiver = make_function.arg(0);
            let (t, u) = (receiver.get_index(0), receiver.get_index(1));
            Value::tuple(&[t, u])
        });
    });
}

#[test]
fn test_tuple() {
    let input = quote! {
        pub(super) struct TupleStruct<T, U>(T, U);
    };

    let expected = quote! {
        impl<__T0, __T1> ::tuple::Tuple<__T0, __T1> for TupleStruct<__T0, __T1> {
            fn swap(__arg0: (__T0, __T1)) -> (__T1, __T0) {
                let __v0 = __arg0;
                let __v1 = __v0.0;
                let __v2 = __v0.1;
                let __v3 = (__v2, __v1);
                __v3
            }

            fn to_tuple(self) -> (__T0, __T1) {
                let __v4 = self;
                let __v5 = __v4.0;
                let __v6 = __v4.1;
                let __v7 = (__v5, __v6);
                __v7
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
