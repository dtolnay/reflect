use reflect::*;

reflect::library! {
    extern crate std {
        mod fmt {
            type Formatter;
            type Result;
            type DebugStruct;

            trait Debug {
                fn fmt(&self, &mut Formatter) -> Result;
            }

            impl Formatter {
                fn debug_struct(&mut self, &str) -> DebugStruct;
            }

            impl DebugStruct {
                fn field(&mut self, &str, &Debug) -> &mut DebugStruct;
                fn finish(&mut self) -> Result;
            }
        }
    }
}

pub fn derive(ex: Execution) {
    ex.make_trait_impl(RUNTIME::std::fmt::Debug, ex.target_type(), |block| {
        block.make_function(RUNTIME::std::fmt::Debug::fmt, debug_fmt);
    });
}

fn debug_fmt(f: MakeFunction) -> Value {
    let receiver = f.arg(0);
    let formatter = f.arg(1);

    let type_name = receiver.get_type_name();

    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(_receiver) => unimplemented!(),
            Struct::Tuple(_receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                let builder = RUNTIME::std::fmt::Formatter::debug_struct
                    .INVOKE(formatter, type_name)
                    .reference_mut();

                for field in receiver.fields() {
                    RUNTIME::std::fmt::DebugStruct::field.INVOKE(
                        builder,
                        field.get_name(),
                        field.get_value(),
                    );
                }

                RUNTIME::std::fmt::DebugStruct::finish.INVOKE(builder)
            }
        },
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(_variant) => unimplemented!(),
            Variant::Tuple(_variant) => unimplemented!(),
            Variant::Struct(_variant) => unimplemented!(),
        }),
    }
}
