use quote::quote;
use reflect::*;

fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::base::Trait, ex.target_type(), |block| {
        block.make_function(RUNTIME::base::Trait::trivial, |make_function| {
            let receiver = make_function.arg(0);
            match receiver.data() {
                Data::Struct(Struct::Struct(receiver)) => {
                    for field in receiver.fields() {
                        field.get_value().get_type_name();
                    }
                }
                _ => unimplemented!(),
            }
            make_function.unit()
        });
    });
}

library! {
    use base {
        trait Trait {
            fn trivial(&self);
        }
    }
}

#[test]
fn test_field_access() {
    let input = quote! {
        struct Test {
            pub test: String,
        }
    };

    let expected = quote! {
        impl ::base::Trait for Test
        {
            fn trivial(&self) {
                let __v3 = ();
                __v3
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
