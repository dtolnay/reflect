use reflect::*;

reflect::library! {
    extern crate std {
        mod fmt {
            type Formatter;
            type Result;
            type DisplayStruct;

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
                RUNTIME::std::write.INVOKE(&[formatter, f.string("{}"), type_name]);

                for field in receiver.fields() {
                    RUNTIME::std::write.INVOKE(&[formatter, f.string("{}"), field.get_value()]);
                }

                f.string("test")
            }
        },
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(_variant) => unimplemented!(),
            Variant::Tuple(_variant) => unimplemented!(),
            Variant::Struct(_variant) => unimplemented!(),
        }),
    }
}
