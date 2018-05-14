I thought Rust doesn't have reflection...?
==========================================

```rust
library! {
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
```

```rust
#[proc_macro_derive(Derive)]
pub fn derive(input: TokenStream) -> TokenStream {
    reflect::derive(input, |ex| {
        ex.make_impl(RUNTIME::std::fmt::Debug, ex.target_type(), |block| {
            block.make_function(RUNTIME::std::fmt::Debug::fmt, debug_fmt);
        });
    })
}
```

```rust
fn debug_fmt(f: MakeFunction) -> Value {
    let receiver = f.arg(0);
    let formatter = f.arg(1);

    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(receiver) => unimplemented!(),
            Struct::Tuple(receiver) => unimplemented!(),
            Struct::Struct(receiver) => unimplemented!(),
        },
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(variant) => unimplemented!(),
            Variant::Tuple(variant) => unimplemented!(),
            Variant::Struct(variant) => unimplemented!(),
        }),
    }
}
```

```rust
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
```

<br>

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>
