use quote::quote;
use reflect::*;

library! {
    use tuple {
        type One;
        type Two;

        trait Tuple {
            fn swap((One, Two)) -> (Two, One);
        }
    }
}

fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::tuple::Tuple, ex.target_type(), |block| {
        block.make_function(RUNTIME::tuple::Tuple::swap, |make_function| {
            let tuple = make_function.arg(0);
            let (i, u) = (tuple.get_tuple_value(0), tuple.get_tuple_value(1));
            Value::tuple(&[u, i])
        });
    });
}

#[test]
fn test_tuple() {
    let input = quote! {
        pub(super) struct TupleStruct;
    };

    let expected = quote! {
        impl ::tuple::Tuple for TupleStruct
        {
            fn swap(__arg0: (::tuple::One, ::tuple::Two)) -> (::tuple::Two, ::tuple::One) {
                let __v0 = __arg0;
                let __v1 = &__v0.0;
                let __v2 = &__v0.1;
                let __v3 = (__v2, __v1);
                __v3
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
