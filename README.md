I thought Rust doesn't have reflection...?
==========================================

*This crate explores what it could look like to tackle the 80% use case of
custom derive macros through a programming model that resembles compile-time
reflection.*

## Motivation

My existing [**`syn`**] and [**`quote`**] libraries approach the problem space
of procedural macros in a super general way and are a good fit for maybe 95% of
use cases. However, the generality comes with the cost of operating at a
relatively low level of abstraction. The macro author is responsible for the
placement of every single angle bracket, lifetime, type parameter, trait bound,
and phantom data. There is a large amount of domain knowledge involved and very
few people can reliably produce robust macros with this approach.

The design explored here focuses on what it would take to make all the edge
cases disappear -- such that if your macro works for the most basic case, then
it also works in every tricky case under the sun.

[**`syn`**]: https://github.com/dtolnay/syn
[**`quote`**]: https://github.com/dtolnay/quote

## Programming model

The idea is that we expose *what looks like* a boring straightforward [runtime
reflection] API such as you might recognize if you have used [reflection in
Java] or [reflection in Go].

The macro author expresses the logic of their macro in terms of this API, using
types like `reflect::Value` to retrieve function arguments and access fields of
data structures and invoke functions and so forth. Importantly, there is no such
thing as a generic type or phantom data in this model. Everything is just a
`reflect::Value` with a type that is conceptually its monomorphized type at
runtime.

Meanwhile the library is tracking the control flow and function invocations to
build up a fully general and robust procedural implementation of the author's
macro. The resulting code will have all the angle brackets and lifetimes and
bounds and phantom types in the right places without the macro author thinking
about any of that.

The reflection API is *just* a means for defining a procedural macro. The
library boils it all away and emits clean Rust source code free of any actual
runtime reflection. Note that this is **not** a statement about compiler
optimizations -- we are not relying on the Rust compiler to do heroic
optimizations on shitty generated code. Literally the source code authored
through the reflection API will be what a seasoned macro author would have
produced simply using `syn` and `quote`.

From the perspective of the person that ends up calling the macro, everything
about how it is called is the same as if the macro were written the old
fashioned way without reflection, and their code compiles exactly as fast and
performs exactly as fast. The advantage is to the macro author for whom
developing and maintaining a robust macro is greatly simplified.

[runtime reflection]: https://en.wikipedia.org/wiki/Reflection_(computer_programming)
[reflection in Java]: https://docs.oracle.com/javase/tutorial/reflect/member/fieldValues.html
[reflection in Go]: https://blog.golang.org/laws-of-reflection

## Demo

This project contains a proof of concept of a compile-time reflection API for
defining custom derives.

The [`tests/debug/`] directory demonstrates a working compilable implementation
of `#[derive(Debug)]` for structs with named fields. The corresponding [test
case] shows what code we emit when deriving `Debug` for a struct `Point` with
two fields; it is equivalent to the code that a handwritten `derive(Debug)`
macro without reflection would emit for the same data structure.

[`tests/debug/`]: https://github.com/dtolnay/reflect/blob/master/tests/debug/mod.rs
[test case]: https://github.com/dtolnay/reflect/blob/master/tests/test_debug.rs

The macro implementation begins with a DSL declaration of the types and
functions that will be required at runtime:

```rust
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
```

There may be additional `extern crate` blocks here if we need to use types from
outside the standard library. For example Serde's `#[derive(Serialize)]` macro
would want to list the `serde` crate, the `Serialize` and `Serializer` types,
and whichever of their methods will possibly be invoked at runtime.

Throughout the rest of the macro implementation, all type information is
statically inferred based on the signatures given in this library declaration.

Next, the macro entry point is an ordinary `proc_macro_derive` function just as
it would be for a derive macro defined any other way.

Once again the reflection API is *just* a means for defining a procedural macro.
Despite what it may look like below, everything written here executes at compile
time. The `reflect` library spits out generated code in an output `TokenStream`
that is compiled into the macro user's crate. This token stream contains no
vestiges of runtime reflection.

```rust
use proc_macro::TokenStream;

// Macro that is called when someone writes derive(MyDebug) on a data structure.
// It returns a fragment of Rust source code (TokenStream) containing an
// implementation of Debug for the input data structure. The macro uses
// compile-time reflection internally, but the generated Debug impl is exactly
// as if this macro were handwritten without reflection.
#[proc_macro_derive(MyDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Feed the tokens describing the data structure into the reflection library
    // for parsing and analysis. We provide a callback that describes what trait
    // impl(s) the reflection library will need to generate code for.
    reflect::derive(input, |ex| {
        // Instruct the library to generate an impl of Debug for the derive
        // macro's target type / Self type.
        ex.make_trait_impl(RUNTIME::std::fmt::Debug, ex.target_type(), |block| {
            // Instruct the library to compile debug_fmt (a function shown
            // below) into the source code for the impl's Debug::fmt method.
            block.make_function(RUNTIME::std::fmt::Debug::fmt, debug_fmt);
        });
    })
}
```

The following looks like a function that does runtime reflection. It receives
function arguments which have the type `reflect::Value` and can pass them
around, pull out their fields, inspect attributes, invoke methods, and so forth.

```rust
use reflect::*;

// This function will get compiled into Debug::fmt, which has this signature:
//
//     fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result
//
fn debug_fmt(f: MakeFunction) -> Value {
    let receiver: reflect::Value = f.arg(0);  // this is `self`
    let formatter: reflect::Value = f.arg(1);

    // The input value may be any of unit struct, tuple struct, ordinary braced
    // struct, or enum.
    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(receiver) => unimplemented!(),
            Struct::Tuple(receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                /* implemented below */
            }
        },
        // For an enum, the active variant of the enum may be any of unit
        // variant, tuple variant, or struct variant.
        Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
            Variant::Unit(variant) => unimplemented!(),
            Variant::Tuple(variant) => unimplemented!(),
            Variant::Struct(variant) => unimplemented!(),
        }),
    }
}
```

In the case of a struct with named fields we use reflection to loop over fields
of the struct and invoke methods of the standard library `Formatter` API to
append each field value into the debug output.

Refer to the [`DebugStruct`] example code in the standard library API
documentation for what this is supposed to do at runtime.

Paths beginning with `RUNTIME::` refer to library signatures declared by the
`library! { ... }` snippet above.

[`DebugStruct`]: https://doc.rust-lang.org/std/fmt/struct.DebugStruct.html

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

The reflection library is able to track how `reflect::Value` objects flow from
one INVOKE to another, and contains a compiler that can compile this data flow
into strongly typed Rust source code in a robust way. In the case of the Debug
derive macro from this demo, when invoked on a braced struct with two fields,

```rust
#[derive(MyDebug)]
struct Point {
    x: i32,
    y: i32,
}
```

the reflection library would emit a trait impl that looks like this:

```rust
// expands to:
impl ::std::fmt::Debug for Point {
    fn fmt(&self, _arg1: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Point { x: ref _v0, y: ref _v1 } => {
                let mut _v2 = ::std::fmt::Formatter::debug_struct(_arg1, "Point");
                let _ = ::std::fmt::DebugStruct::field(&mut _v2, "x", _v0);
                let _ = ::std::fmt::DebugStruct::field(&mut _v2, "y", _v1);
                let _v3 = ::std::fmt::DebugStruct::finish(&mut _v2);
                _v3
            }
        }
    }
}
```

This generated code is what ends up running at runtime. Notice that there is no
reflection. In fact this is pretty much identical to what the standard library's
built-in `derive(Debug)` macro produces for the same data structure.

## Robustness and how things go wrong

I mentioned above about how implementing robust macros simply using `syn` and
`quote` is quite challenging.

The example I like to use is taking a single struct field and temporarily
wrapping it in a new struct. This is a real life use case drawn from how
`serde_derive` handles `serialize_with` attributes. Conceptually:

```rust
let input: DeriveInput = syn::parse(...).unwrap();

// Pull out one of the field types.
let type_of_field_x: syn::Type = /* ... */;

quote! {
    // Very not robust.
    struct Wrapper<'a> {
        x: &'a #type_of_field_x,
    }

    Wrapper { x: &self.x }
}
```

Making the `quote!` part of this simply generate compilable code for all
possible values of `type_of_field_x` is extremely involved. The macro author
needs to consider and handle all of the following in order to make this work
reliably:

- Lifetime parameters used by `type_of_field_x`,
- Type parameters used by `type_of_field_x`,
- Associated types used by `type_of_field_x`,
- Where-clauses on `input` that constrain any of the above,
- Similarly, trait bounds on type parameters of `input`,
- Where-clauses or bounds affecting any *other* fields of `input`,
- Type parameter defaults on `input` that need to be stripped.

In contrast, the `reflect` library will be able to get it right every single
time with much less thought from the macro author. Possibly as trivial as:

```rust
let wrapper: reflect::Type = reflect::new_struct_type();

wrapper.instantiate(vec![input.get_field("x").reference()])
```

## Remaining work

In its current state the proof of concept generates just barely working code for
our simple `Debug` derive. The `reflect` library needs more work to produce
robust code in the presence of lifetimes and generic parameters, and for library
signatures involving more complicated types.

Crucially all remaining work should happen without touching the code of our
`Debug` derive. The promise of `reflect` is that if the macro works for the most
basic cases (which the code above already does) then it also works in all the
edge cases. From here it is `reflect`'s responsibility to compile the dead
simple reflection-like `reflect::Value` object manipulations into a fully
general and robust procedural macro.

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
