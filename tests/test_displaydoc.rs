// This test implements a Display derive resembling the displaydoc crate:
// https://github.com/yaahc/displaydoc

use quote::quote;
use reflect::*;
use syn::{Attribute, Lit, Meta, MetaNameValue};

reflect::library! {
    extern crate std {
        mod fmt {
            type Formatter;
            type Result;

            trait Display {
                fn fmt(&self, &mut Formatter) -> Result;
            }
        }

        macro write;
    }
}

fn derive_displaydoc(ex: Execution) {
    ex.make_trait_impl(RUNTIME::std::fmt::Display, ex.target_type(), |block| {
        block.make_function(RUNTIME::std::fmt::Display::fmt, display_fmt);
    });
}

fn display_fmt(f: MakeFunction) -> Value {
    let receiver = f.arg(0);
    let formatter = f.arg(1);

    let type_name = receiver.get_type_name();
    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(_receiver) => unimplemented!(),
            Struct::Tuple(_receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                let num_fields = receiver.fields().count();
                let attrs = receiver.attrs();
                let doc_comment = extract_doc_comment(attrs);
                let fmt_string = format!(
                    "{} ",
                    doc_comment.as_ref().map(String::as_str).unwrap_or("{}")
                );

                let mut last_write = if !fmt_string.contains('{') {
                    RUNTIME::std::write.INVOKE(&[formatter, f.string(&fmt_string)])
                } else {
                    RUNTIME::std::write.INVOKE(&[formatter, f.string(&fmt_string), type_name])
                };

                for (i, field) in receiver.fields().enumerate() {
                    let doc_comment = extract_doc_comment(field.get_attrs());
                    let mut fmt_string = doc_comment.unwrap_or_else(|| "{}".to_owned());

                    // Add space after field display string, except for the last one
                    if i + 1 != num_fields {
                        fmt_string.push(' ');
                    }

                    last_write = RUNTIME::std::write.INVOKE(&[
                        formatter,
                        f.string(&fmt_string),
                        field.get_value(),
                    ])
                }

                last_write
            }
        },
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(_variant) => unimplemented!(),
            Variant::Tuple(_variant) => unimplemented!(),
            Variant::Struct(_variant) => unimplemented!(),
        }),
    }
}

fn extract_doc_comment(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path.is_ident("doc") {
            if let Ok(meta) = attr.parse_meta() {
                if let Meta::NameValue(MetaNameValue {
                    lit: Lit::Str(lit), ..
                }) = meta
                {
                    return Some(lit.value().trim().to_owned());
                }
            }
        }
    }

    None
}

#[test]
fn test_displaydoc() {
    let input = quote! {
        /// Point in space:
        struct Point {
            /// x: {},
            x: i32,

            /// y: {}
            y: i32,
        }
    };

    let expected = quote! {
        impl ::std::fmt::Display for Point {
            fn fmt(&self, __arg0: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let __v0 = self;
                let __v1 = __arg0;
                let __v3 = &__v0.x;
                let __v4 = &__v0.y;
                let _ = write!(__v1, "Point in space: ");
                let _ = write!(__v1, "x: {}, ", __v3);
                let __v10 = write!(__v1, "y: {}", __v4);
                __v10
            }
        }
    };

    let actual = reflect::derive(input, derive_displaydoc);
    assert_eq!(actual.to_string(), expected.to_string());
}
