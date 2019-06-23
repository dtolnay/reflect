use crate::CompleteFunction;
use crate::CompleteImpl;
use crate::Data;
use crate::Enum;
use crate::Execution;
use crate::Field;
use crate::Program;
use crate::Struct;
use crate::StructStruct;
use crate::Tracker;
use crate::TupleStruct;
use crate::Type;
use crate::TypeNode;
use crate::UnitStruct;
use crate::WipFunction;
use crate::WipImpl;

use proc_macro2::{self, TokenStream};
use syn;

pub fn derive<TokenStream>(input: TokenStream, run: fn(Execution)) -> TokenStream
where
    TokenStream: Into<proc_macro2::TokenStream> + From<proc_macro2::TokenStream>,
{
    let input = input.into();
    let output = derive2(input, run);
    output.into()
}

fn derive2(input: TokenStream, run: fn(Execution)) -> TokenStream {
    let input = syn::parse2(input).unwrap();
    let ty = syn_to_type(input);

    let tracker = Tracker::new();
    run(Execution {
        ty: &ty,
        tracker: &tracker,
    });

    let program = tracker_to_program(tracker);
    program.compile()
}

fn syn_to_type(input: syn::DeriveInput) -> Type {
    Type(TypeNode::DataStructure {
        name: input.ident.to_string(),
        data: match input.data {
            syn::Data::Struct(data) => {
                match data.fields {
                    syn::Fields::Named(fields) => {
                        Data::Struct(Struct::Struct(StructStruct {
                            fields: fields
                                .named
                                .into_iter()
                                .map(|field| {
                                    Field {
                                        name: field.ident.unwrap().to_string(),
                                        // FIXME convert syn field type
                                        element: Type::unit().0,
                                    }
                                })
                                .collect(),
                        }))
                    }
                    syn::Fields::Unnamed(fields) => {
                        Data::Struct(Struct::Tuple(TupleStruct {
                            fields: fields
                                .unnamed
                                .into_iter()
                                .enumerate()
                                .map(|(i, field)| {
                                    Field {
                                        // FIXME store field index
                                        name: i.to_string(),
                                        // FIXME convert syn field type
                                        element: Type::unit().0,
                                    }
                                })
                                .collect(),
                        }))
                    }
                    syn::Fields::Unit => Data::Struct(Struct::Unit(UnitStruct { private: () })),
                }
            }
            syn::Data::Enum(data) => {
                // FIXME convert enum variants
                Data::Enum(Enum {
                    variants: Vec::new(),
                })
            }
            syn::Data::Union(_) => unimplemented!("union"),
        },
    })
}

fn tracker_to_program(tracker: Tracker) -> Program {
    Program {
        crates: tracker.crates.borrow().clone(),
        impls: tracker
            .impls
            .borrow()
            .iter()
            .map(|imp| {
                let imp: WipImpl = imp.borrow().clone();
                CompleteImpl {
                    of: imp.of,
                    ty: imp.ty,
                    functions: imp
                        .functions
                        .into_iter()
                        .map(|function| {
                            let function: WipFunction = function.borrow().clone();
                            let values = function.values.borrow().clone();
                            let invokes = function.invokes.borrow().clone();
                            CompleteFunction {
                                self_ty: function.self_ty,
                                f: function.f,
                                values,
                                invokes,
                                ret: function.ret,
                            }
                        })
                        .collect(),
                }
            })
            .collect(),
    }
}
