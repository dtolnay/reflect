#![allow(unused_variables, dead_code)]
#![cfg_attr(
    feature = "cargo-clippy",
    allow(needless_pass_by_value, new_without_default, for_loop_over_option)
)]

#[macro_use]
extern crate ref_cast;

#[macro_use]
extern crate quote;

extern crate proc_macro2;
extern crate syn;

pub mod runtime;

mod array;
mod compiler;
mod data;
mod derive;
mod execution;
mod field;
mod function;
mod generics;
mod ident;
mod index;
mod macros;
mod map;
mod module;
mod node;
mod print;
mod signature;
mod ty;
mod value;
mod wip;

pub use data::{
    Data, Enum, Struct, StructStruct, StructVariant, TupleStruct, TupleVariant, UnitStruct,
    UnitVariant, Variant,
};
pub use derive::derive;
pub use execution::Execution;
pub use field::{Field, Fields};
pub use function::Function;
pub use generics::Generics;
pub use module::Module;
pub use signature::{Receiver, Signature};
pub use ty::Type;
pub use value::Value;
pub use wip::{MakeFunction, MakeImpl};

use array::Array;
use compiler::{CompleteFunction, CompleteImpl, Program};
use execution::Tracker;
use ident::Ident;
use index::{InvokeRef, Push, ValueRef};
use node::ValueNode;
use print::Print;
use runtime::{RuntimeFunction, RuntimeType};
use wip::{Invoke, WipFunction, WipImpl};
