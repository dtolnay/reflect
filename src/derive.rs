use crate::{
    Accessor, CompleteFunction, CompleteImpl, Data, Enum, Execution, Field, Generics, Ident,
    Program, Struct, StructStruct, Tracker, TupleStruct, Type, TypeNode, UnitStruct, WipFunction,
};
use proc_macro2::TokenStream;
use syn::DeriveInput;

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

fn syn_to_type(input: DeriveInput) -> Type {
    Type(TypeNode::DataStructure {
        name: Ident::from(input.ident),
        generics: Generics::syn_to_generics(input.generics),
        data: match input.data {
            syn::Data::Struct(data) => match data.fields {
                syn::Fields::Named(fields) => Data::Struct(Struct::Struct(StructStruct {
                    fields: fields
                        .named
                        .into_iter()
                        .map(|field| Field {
                            attrs: field.attrs,
                            accessor: Accessor::Name(Ident::from(field.ident.unwrap())),
                            element: Type::syn_to_type(field.ty),
                        })
                        .collect(),
                    attrs: input.attrs,
                })),
                syn::Fields::Unnamed(fields) => Data::Struct(Struct::Tuple(TupleStruct {
                    fields: fields
                        .unnamed
                        .into_iter()
                        .enumerate()
                        .map(|(i, field)| Field {
                            attrs: field.attrs,
                            accessor: Accessor::Index(i),
                            element: Type::syn_to_type(field.ty),
                        })
                        .collect(),
                    attrs: input.attrs,
                })),
                syn::Fields::Unit => Data::Struct(Struct::Unit(UnitStruct { attrs: input.attrs })),
            },
            syn::Data::Enum(data) => {
                // FIXME convert enum variants
                Data::Enum(Enum {
                    variants: Vec::new(),
                    attrs: input.attrs,
                })
            }
            syn::Data::Union(_) => unimplemented!("union"),
        },
    })
}

fn tracker_to_program(tracker: Tracker) -> Program {
    Program {
        crates: tracker.crates.into_inner(),
        impls: tracker
            .impls
            .into_inner()
            .into_iter()
            .map(|imp| CompleteImpl {
                trait_ty: imp.trait_ty,
                ty: imp.ty,
                functions: imp
                    .functions
                    .into_inner()
                    .into_iter()
                    .map(|function| {
                        let function: WipFunction = function;
                        let values = function.values;
                        let invokes = function.invokes;
                        let macros = function.macros;
                        CompleteFunction {
                            self_ty: function.self_ty,
                            f: function.f,
                            values,
                            invokes,
                            macros,
                            ret: function.ret,
                        }
                    })
                    .collect(),
            })
            .collect(),
    }
}
