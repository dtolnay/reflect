use quote::quote;
use reflect::*;

fn derive(ex: Execution) {
    // Checks from #[trivial(skip)] attribute on a field
    fn should_skip(field: &Field<Value>) -> bool {
        field.get_attrs().iter().any(|attr| {
            syn::custom_keyword!(skip);
            attr.path.is_ident("trivial") && attr.parse_args::<skip>().is_ok()
        })
    }

    ex.make_trait_impl(RUNTIME::base::Trait, ex.target_type(), |block| {
        block.make_function(RUNTIME::base::Trait::trivial, |make_function| {
            let receiver = make_function.arg(0);
            match receiver.data() {
                Data::Struct(Struct::Struct(receiver)) => {
                    for field in receiver.fields() {
                        if should_skip(&field) {
                            continue;
                        }

                        RUNTIME::base::FieldAccessor::access_field.INVOKE(field.get_value());
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
        type FieldAccessor;
        impl FieldAccessor {
            fn access_field(&str);
        }

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

            #[trivial(skip)]
            pub skip_me: String,
        }
    };

    // skip_me field should be skipped from the accessing
    let expected = quote! {
        impl ::base::Trait for Test {
            fn trivial<'__a1>(&'__a1 self) {
                let __v0 = self;
                let __v1 = &__v0.test;
                let _ = ::base::FieldAccessor::access_field(__v1);
            }
        }
    };

    let output = reflect::derive(input, derive);
    assert_eq!(output.to_string(), expected.to_string());
}
