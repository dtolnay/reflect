use reflect::*;
use syn::Meta;

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

pub fn derive(ex: Execution) {
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
                let fmt_string = format!("{} ", doc_comment.unwrap_or("{}".to_owned()));

                let mut last_write = if !fmt_string.contains("{") {
                    RUNTIME::std::write.INVOKE(&[formatter, f.string(&fmt_string)])
                } else {
                    RUNTIME::std::write.INVOKE(&[formatter, f.string(&fmt_string), type_name])
                };

                for (i, field) in receiver.fields().enumerate() {
                    let doc_comment = extract_doc_comment(field.get_attrs());
                    let mut fmt_string = doc_comment.unwrap_or("{}".to_owned());

                    // Add space after field display string, except for the last one
                    if i + 1 != num_fields {
                        fmt_string = format!("{} ", fmt_string);
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

fn extract_doc_comment(attrs: &[syn::Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path.is_ident("doc") {
            if let Ok(meta) = attr.parse_meta() {
                if let Meta::NameValue(syn::MetaNameValue {
                    lit: syn::Lit::Str(lit),
                    ..
                }) = meta
                {
                    return Some(lit.value().trim().to_owned());
                }
            }
        }
    }

    None
}
