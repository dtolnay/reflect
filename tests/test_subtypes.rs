use quote::quote;
use reflect::*;

library! {
    use subtypes {
        type Subtypes;
        type SameLifetime<'a, T>;

        impl Subtypes {
            fn sub1<'a, 'b, 'c, T, U>(&'a T, &'b U) -> &'c U where 'a: 'b;
            fn sub2<'c, 'b: 'c, 'd, U, V>(&'b U, &'c V) -> &'d V;
            fn sub3<'c, 'd, V, W>(&'c V, &'d W) where 'c: 'd, 'd: 'static;
            fn ident_elided<T>(&T) -> &T;
            fn consume_static<T>(&'static T);
        }

        trait CallSubtypes<'a, 'b, T, U> {
            fn call_subtypes(&'a mut T, &'b mut U);
        }

        trait HasSubtypes<'a, 'b, T> where 'a: 'b {
            fn has_subtypes(&'a T, &'b T) -> &'b T;
        }

        trait IdentElided<'a> {
            fn ident_elided(&'a self);
        }

        impl<'a, T> SameLifetime<'a, T> {
            fn same_lifetime(&'a T, &'a T) -> &'a T;
        }
    }
}

fn derive_subtypes(ex: Execution) {
    ex.make_trait_impl(RUNTIME::subtypes::CallSubtypes, ex.target_type(), |block| {
        block.make_function(
            RUNTIME::subtypes::CallSubtypes::call_subtypes,
            |make_function| {
                let a = make_function.arg(0);
                let b = make_function.arg(1);
                let c = RUNTIME::subtypes::Subtypes::sub1.INVOKE(a, b);
                let stat = RUNTIME::subtypes::Subtypes::sub2.INVOKE(b, c);
                RUNTIME::subtypes::Subtypes::sub3.INVOKE(c, stat)
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
        impl<__T0, __T1> ::subtypes::CallSubtypes<'static, 'static, __T0, __T1> for Trivial
        {
            fn call_subtypes(__arg0: &'static mut __T0, __arg1: &'static mut __T1) {
                let __v0 = __arg0;
                let __v1 = __arg1;
                let __v2 = ::subtypes::Subtypes::sub1(__v0, __v1);
                let __v3 = ::subtypes::Subtypes::sub2(__v1, __v2);
                let _ = ::subtypes::Subtypes::sub3(__v2, __v3);
            }
        }
    };

    let output = reflect::derive(input, derive_subtypes);
    assert_eq!(output.to_string(), expected.to_string());
}

fn derive_keep_subtypes(ex: Execution) {
    ex.make_trait_impl(RUNTIME::subtypes::HasSubtypes, ex.target_type(), |block| {
        block.make_function(
            RUNTIME::subtypes::HasSubtypes::has_subtypes,
            |make_function| {
                let a = make_function.arg(0);
                let b = make_function.arg(1);
                RUNTIME::subtypes::SameLifetime::same_lifetime.INVOKE(a, b)
            },
        );
    });
}

/// Test if HasSubtypes keeps its subtyping relationship
#[test]
fn keep_subtypes() {
    let input = quote! {
        struct Trivial;
    };

    let expected = quote! {
        impl<'__a1, '__a2, __T0> ::subtypes::HasSubtypes<'__a1, '__a2, __T0> for Trivial
        where
            '__a1: '__a2,
        {
            fn has_subtypes(__arg0: &'__a1 __T0, __arg1: &'__a2 __T0) -> &'__a2 __T0 {
                let __v0 = __arg0;
                let __v1 = __arg1;
                let __v2 = ::subtypes::SameLifetime::same_lifetime(__v0, __v1);
                __v2
            }
        }
    };

    let output = reflect::derive(input, derive_keep_subtypes);
    assert_eq!(output.to_string(), expected.to_string());
}

fn derive_elided_static(ex: Execution) {
    ex.make_trait_impl(RUNTIME::subtypes::IdentElided, ex.target_type(), |block| {
        block.make_function(
            RUNTIME::subtypes::IdentElided::ident_elided,
            |make_function| {
                let receiver = make_function.arg(0);
                let receiver = RUNTIME::subtypes::Subtypes::ident_elided.INVOKE(receiver);
                RUNTIME::subtypes::Subtypes::consume_static.INVOKE(receiver)
            },
        );
    });
}

// FIXME: infer 'static lifetime
#[test]
fn elided_static() {
    let input = quote! {
        struct Trivial;
    };

    let expected = quote! {
        impl ::subtypes::IdentElided<'static> for Trivial {
            fn ident_elided(&'static self) {
                let __v0 = self;
                let __v1 = ::subtypes::Subtypes::ident_elided(__v0);
                let _ = ::subtypes::Subtypes::consume_static(__v1);
            }
        }
    };

    let output = reflect::derive(input, derive_elided_static);
    assert_eq!(output.to_string(), expected.to_string());
}
