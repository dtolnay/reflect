use quote::quote;
use reflect::*;

library! {
    use subtypes {
        type Subtypes;

        impl Subtypes {
            fn sub1<'a, 'b, T, U>(&'a T, &'b U) -> &'b U where 'a: 'b;
            fn sub2<'b, 'c, U, V>(&'b U, &'c V) -> &'c V where 'b: 'c, 'c: 'b;
            fn sub3<'c, 'd, V, W>(&'c V, &'d W) where 'c: 'd, 'd: 'static;
        }

        trait CallSubtypes<'a, 'b, T, U> {
            fn call_subtypes(&'a mut T, &'b mut U);
        }
    }
}

fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::subtypes::CallSubtypes, ex.target_type(), |block| {
        block.make_function(
            RUNTIME::subtypes::CallSubtypes::call_subtypes,
            |make_function| {
                let a = make_function.arg(0);
                let b = make_function.arg(1);
                let c = RUNTIME::subtypes::Subtypes::sub1.INVOKE(a, b);
                let stat = RUNTIME::subtypes::Subtypes::sub2.INVOKE(b, c);
                RUNTIME::subtypes::Subtypes::sub3.INVOKE(c, stat);
                make_function.unit()
            },
        );
    });
}

#[test]
fn test_transitive_closure() {
    let input = quote! {
        struct Trivial;
    };

    let expected = quote! {
        impl<'__a1, '__a2, __T0, __T1> ::subtypes::CallSubtypes<'__a1, '__a2, __T0, __T1> for Trivial
        where
            '__a1: '__a2,
            '__a1: 'static,
            '__a2: 'static,
        {
            fn call_subtypes(__arg0: &'__a1 mut __T0, __arg1: &'__a2 mut __T1) {
                let __v0 = __arg0;
                let __v1 = __arg1;
                let __v2 = ::subtypes::Subtypes::sub1(__v0, __v1);
                let __v3 = ::subtypes::Subtypes::sub2(__v1, __v2);
                let _ = ::subtypes::Subtypes::sub3(__v2, __v3);
                let __v5 = ();
                __v5
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
