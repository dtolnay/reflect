//! I thought Rust doesn't have reflection...?
//! ==========================================
//!
//! *This crate explores what it could look like to tackle the 80% use case of
//! custom derive macros through a programming model that resembles compile-time
//! reflection.*
//!
//! # Motivation
//!
//! My existing [**`syn`**] and [**`quote`**] libraries approach the problem space
//! of procedural macros in a super general way and are a good fit for maybe 95% of
//! use cases. However, the generality comes with the cost of operating at a
//! relatively low level of abstraction. The macro author is responsible for the
//! placement of every single angle bracket, lifetime, type parameter, trait bound,
//! and phantom data. There is a large amount of domain knowledge involved and very
//! few people can reliably produce robust macros with this approach.
//!
//! The design explored here focuses on what it would take to make all the edge
//! cases disappear -- such that if your macro works for the most basic case, then
//! it also works in every tricky case under the sun.
//!
//! [**`syn`**]: https://github.com/dtolnay/syn
//! [**`quote`**]: https://github.com/dtolnay/quote
//!
//! # Programming model
//!
//! The idea is that we expose *what looks like* a boring straightforward [runtime
//! reflection] API such as you might recognize if you have used [reflection in
//! Java] or [reflection in Go].
//!
//! The macro author expresses the logic of their macro in terms of this API, using
//! types like `reflect::Value` to retrieve function arguments and access fields of
//! data structures and invoke functions and so forth. Importantly, there is no such
//! thing as a generic type or phantom data in this model. Everything is just a
//! `reflect::Value` with a type that is conceptually its monomorphized type at
//! runtime.
//!
//! Meanwhile the library is tracking the control flow and function invocations to
//! build up a fully general and robust procedural implementation of the author's
//! macro. The resulting code will have all the angle brackets and lifetimes and
//! bounds and phantom types in the right places without the macro author thinking
//! about any of that.
//!
//! The reflection API is *just* a means for defining a procedural macro. The
//! library boils it all away and emits clean Rust source code free of any actual
//! runtime reflection. Note that this is **not** a statement about compiler
//! optimizations -- we are not relying on the Rust compiler to do heroic
//! optimizations on shitty generated code. Literally the source code authored
//! through the reflection API will be what a seasoned macro author would have
//! produced simply using `syn` and `quote`.
//!
//! [runtime reflection]: https://en.wikipedia.org/wiki/Reflection_(computer_programming)
//! [reflection in Java]: https://docs.oracle.com/javase/tutorial/reflect/member/fieldValues.html
//! [reflection in Go]: https://blog.golang.org/laws-of-reflection
//!
//! # Demo
//!
//! This project contains a proof of concept of a compile-time reflection API for
//! defining custom derives.
//!
//! The [`tests/debug/`] directory demonstrates a working compilable implementation
//! of `#[derive(Debug)]` for structs with named fields.
//!
//! [`tests/debug/`]: https://github.com/dtolnay/reflect/blob/master/tests/debug/mod.rs
//!
//! We begin with a DSL declaration of the types and functions that will be required
//! at runtime. There may be additional `extern crate` blocks here if we need to use
//! types from outside the standard library. For example Serde's
//! `#[derive(Serialize)]` macro would want to list the `serde` crate, the
//! `Serialize` and `Serializer` types, and whichever of their methods will possibly
//! be invoked at runtime.
//!
//! ```
//! extern crate reflect;
//!
//! reflect::library! {
//!     extern crate std {
//!         mod fmt {
//!             type Formatter;
//!             type Result;
//!             type DebugStruct;
//!
//!             trait Debug {
//!                 fn fmt(&self, &mut Formatter) -> Result;
//!             }
//!
//!             impl Formatter {
//!                 fn debug_struct(&mut self, &str) -> DebugStruct;
//!             }
//!
//!             impl DebugStruct {
//!                 fn field(&mut self, &str, &Debug) -> &mut DebugStruct;
//!                 fn finish(&mut self) -> Result;
//!             }
//!         }
//!     }
//! }
//! #
//! # fn main() {}
//! ```
//!
//! Next, the macro entry point is an ordinary `proc_macro_derive` function just as
//! it would be for a derive macro defined any other way.
//!
//! Once again the reflection API is *just* a means for defining a procedural macro.
//! Despite what it may look like below, everything written here executes at compile
//! time. The `reflect` library spits out generated code in an output `TokenStream`
//! that is compiled into the macro user's crate. This token stream contains no
//! vestiges of runtime reflection.
//!
//! ```
//! # extern crate reflect;
//! #
//! # reflect::library! {
//! #     extern crate std {
//! #         mod fmt {
//! #             type Formatter;
//! #             type Result;
//! #
//! #             trait Debug {
//! #                 fn fmt(&self, &mut Formatter) -> Result;
//! #             }
//! #         }
//! #     }
//! # }
//! #
//! extern crate proc_macro;
//! use self::proc_macro::TokenStream;
//!
//! # macro_rules! ignore {
//! #     ($($tt:tt)*) => {};
//! # }
//! # ignore! {
//! #[proc_macro_derive(MyDebug)]
//! # }
//! pub fn derive(input: TokenStream) -> TokenStream {
//!     reflect::derive(input, |ex| {
//!         ex.make_trait_impl(RUNTIME::std::fmt::Debug, ex.target_type(), |block| {
//!             block.make_function(RUNTIME::std::fmt::Debug::fmt, debug_fmt);
//!         });
//!     })
//! }
//! #
//! # fn debug_fmt(f: reflect::MakeFunction) -> reflect::Value {
//! #     unimplemented!()
//! # }
//! #
//! # fn main() {}
//! ```
//!
//! The following looks like a function that does runtime reflection. It receives
//! function arguments which have the type `reflect::Value` and can pass them
//! around, pull out their fields, inspect attributes, invoke methods, and so forth.
//!
//! ```
//! use reflect::*;
//!
//! fn debug_fmt(f: MakeFunction) -> Value {
//!     let receiver = f.arg(0);
//!     let formatter = f.arg(1);
//!
//!     match receiver.data() {
//!         Data::Struct(receiver) => match receiver {
//!             Struct::Unit(receiver) => unimplemented!(),
//!             Struct::Tuple(receiver) => unimplemented!(),
//!             Struct::Struct(receiver) => {
//!                 /* implemented below */
//! # unimplemented!()
//!             }
//!         },
//!         Data::Enum(receiver) => receiver.match_variant(|variant| match variant {
//!             Variant::Unit(variant) => unimplemented!(),
//!             Variant::Tuple(variant) => unimplemented!(),
//!             Variant::Struct(variant) => unimplemented!(),
//!         }),
//!     }
//! }
//! ```
//!
//! In the case of a struct with named fields we use reflection to loop over fields
//! of the struct and invoke methods of the standard library `Formatter` API to
//! append each field value into the debug output.
//!
//! Refer to the [`DebugStruct`] example code in the standard library API
//! documentation for what this is supposed to do at runtime.
//!
//! Paths beginning with `RUNTIME::` refer to library signatures declared by the
//! `library! { ... }` snippet above.
//!
//! [`DebugStruct`]: https://doc.rust-lang.org/std/fmt/struct.DebugStruct.html
//!
//! ```
//! # extern crate reflect;
//! #
//! # use reflect::*;
//! #
//! # reflect::library! {
//! #     extern crate std {
//! #         mod fmt {
//! #             type Formatter;
//! #             type Result;
//! #             type DebugStruct;
//! #
//! #             trait Debug {
//! #                 fn fmt(&self, &mut Formatter) -> Result;
//! #             }
//! #
//! #             impl Formatter {
//! #                 fn debug_struct(&mut self, &str) -> DebugStruct;
//! #             }
//! #
//! #             impl DebugStruct {
//! #                 fn field(&mut self, &str, &Debug) -> &mut DebugStruct;
//! #                 fn finish(&mut self) -> Result;
//! #             }
//! #         }
//! #     }
//! # }
//! #
//! # fn debug_fmt<'a>(
//! #     receiver: StructStruct<Value>,
//! #     formatter: Value,
//! #     type_name: Value,
//! # ) -> Value {
//! let builder = RUNTIME::std::fmt::Formatter::debug_struct
//!     .INVOKE(formatter, type_name)
//!     .reference_mut();
//!
//! for field in receiver.fields() {
//!     RUNTIME::std::fmt::DebugStruct::field.INVOKE(
//!         builder,
//!         field.get_name(),
//!         field.get_value(),
//!     );
//! }
//!
//! RUNTIME::std::fmt::DebugStruct::finish.INVOKE(builder)
//! # }
//! #
//! # fn main() {}
//! ```
//!
//! # Robustness and how things go wrong
//!
//! I mentioned above about how implementing robust macros simply using `syn` and
//! `quote` is quite challenging.
//!
//! The example I like to use is taking a single struct field and temporarily
//! wrapping it in a new struct. This is a real life use case drawn from how
//! `serde_derive` handles `serialize_with` attributes. Conceptually:
//!
//! ```
//! # macro_rules! ignore {
//! #     ($($tt:tt)*) => {}
//! # }
//! #
//! # ignore! {
//! let input: DeriveInput = syn::parse(...).unwrap();
//!
//! // Pull out one of the field types.
//! let type_of_field_x: syn::Type = /* ... */;
//!
//! quote! {
//!     // Very not robust.
//!     struct Wrapper<'a> {
//!         x: &'a #type_of_field_x,
//!     }
//!
//!     Wrapper { x: &self.x }
//! }
//! # }
//! ```
//!
//! Making the `quote!` part of this simply generate compilable code for all
//! possible values of `type_of_field_x` is extremely involved. The macro author
//! needs to consider and handle all of the following in order to make this work
//! reliably:
//!
//! - Lifetime parameters used by `type_of_field_x`,
//! - Type parameters used by `type_of_field_x`,
//! - Associated types used by `type_of_field_x`,
//! - Where-clauses on `input` that constrain any of the above,
//! - Similarly, trait bounds on type parameters of `input`,
//! - Where-clauses or bounds affecting any *other* fields of `input`,
//! - Type parameter defaults on `input` that need to be stripped.
//!
//! In contrast, the `reflect` library will be able to get it right every single
//! time with much less thought from the macro author. Possibly as trivial as:
//!
//! ```
//! # macro_rules! ignore {
//! #     ($($tt:tt)*) => {}
//! # }
//! #
//! # ignore! {
//! let wrapper: reflect::Type = reflect::new_struct_type();
//!
//! wrapper.instantiate(vec![input.get_field("x").reference()])
//! # }
//! ```
//!
//! # Remaining work
//!
//! In its current state the proof of concept generates just barely working code for
//! our simple `Debug` derive. The `reflect` library needs more work to produce
//! robust code in the presence of lifetimes and generic parameters, and for library
//! signatures involving more complicated types.
//!
//! Crucially all remaining work should happen without touching the code of our
//! `Debug` derive. The promise of `reflect` is that if the macro works for the most
//! basic cases (which the code above already does) then it also works in all the
//! edge cases. From here it is `reflect`'s responsibility to compile the dead
//! simple reflection-like `reflect::Value` object manipulations into a fully
//! general and robust procedural macro.

#![doc(html_root_url = "https://docs.rs/reflect/0.0.4")]
#![warn(clippy::explicit_iter_loop, clippy::doc_markdown)]
#![allow(unused_variables, dead_code)]
#![allow(
    clippy::needless_pass_by_value,
    clippy::new_without_default,
    clippy::large_enum_variant,
    clippy::trivially_copy_pass_by_ref
)]

#[doc(hidden)]
pub use reflect_internal::*;

pub mod runtime;

mod attr;
mod compiler;
mod data;
mod derive;
mod execution;
mod field;
mod function;
mod generics;
mod global_data;
mod ident;
mod index;
mod map;
mod module;
mod node;
mod parent;
mod path;
mod print;
mod signature;
mod trait_inference;
mod ty;
mod value;
mod wip;

pub use crate::data::{
    Data, Enum, Struct, StructStruct, StructVariant, TupleStruct, TupleVariant, UnitStruct,
    UnitVariant, Variant,
};
pub use crate::derive::derive;
pub use crate::execution::Execution;
pub use crate::field::{Field, Fields};
pub use crate::function::Function;
pub use crate::generics::{GenericArguments, Generics, SynParamMap};
pub use crate::module::Module;
pub use crate::parent::{Parent, ParentBuilder, ParentKind};
pub use crate::path::Path;
pub use crate::signature::Signature;
pub use crate::ty::Type;
pub use crate::value::Value;
pub use crate::wip::{MakeFunction, MakeImpl};

use crate::compiler::{CompleteFunction, CompleteImpl, Program};
use crate::execution::Tracker;
use crate::field::Accessor;
use crate::generics::{
    GenericArgument, GenericConstraint, GenericParam, Lifetime, LifetimeDef, ParamMap,
    PredicateType, TraitBound, TypeParam, TypeParamBound,
};
use crate::global_data::{
    GlobalBorrow, GlobalCounter, GlobalPush, INVOKES, LIFETIMES, MACROS, STATIC_LIFETIME,
    TYPE_PARAMS, VALUES,
};
use crate::ident::Ident;
use crate::index::{InvokeRef, MacroInvokeRef, Push, TypeEqualitySetRef, TypedIndex, ValueRef};
use crate::node::ValueNode;
use crate::path::{AngleBracketedGenericArguments, PathArguments, SimplePath};
use crate::print::Print;
use crate::runtime::{RuntimeFunction, RuntimeTrait, RuntimeType};
use crate::signature::Receiver;
use crate::trait_inference::{TraitInferenceResult, TypeEqualitySet};
use crate::ty::{DataStructure, TypeNode};
use crate::wip::{Invoke, MacroInvoke, WipFunction, WipImpl};
