use crate::{
    global_data, Accessor, CompleteFunction, CompleteImpl, Data, Enum, Execution, Field, Generics,
    Ident, Program, Struct, StructStruct, Tracker, TraitInferenceResult, TupleStruct, Type,
    TypeNode, UnitStruct, WipFunction, WipImpl,
};
use proc_macro2::TokenStream;
use syn::DeriveInput;

pub fn derive<TokenStream>(input: TokenStream, run: fn(Execution)) -> TokenStream
where
    TokenStream: Into<proc_macro2::TokenStream> + From<proc_macro2::TokenStream>,
{
    let input = input.into();
    let output = derive2(input, run);
    global_data::clear();
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
    let attrs: Vec<_> = input
        .attrs
        .into_iter()
        .map(std::convert::Into::into)
        .collect();
    let mut generics = Generics::syn_to_generics(input.generics);

    let data = match input.data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(fields) => Data::Struct(Struct::Struct(StructStruct {
                fields: fields
                    .named
                    .into_iter()
                    .map(|field| Field {
                        attrs: field.attrs,
                        accessor: Accessor::Name(Ident::from(field.ident.unwrap())),
                        element: Type::syn_to_type(field.ty, &mut generics.param_map),
                    })
                    .collect(),
                attrs,
            })),
            syn::Fields::Unnamed(fields) => Data::Struct(Struct::Tuple(TupleStruct {
                fields: fields
                    .unnamed
                    .into_iter()
                    .enumerate()
                    .map(|(i, field)| Field {
                        attrs: field.attrs,
                        accessor: Accessor::Index(i),
                        element: Type::syn_to_type(field.ty, &mut generics.param_map),
                    })
                    .collect(),
                attrs,
            })),
            syn::Fields::Unit => Data::Struct(Struct::Unit(UnitStruct { attrs })),
        },
        syn::Data::Enum(data) => {
            // FIXME convert enum variants
            Data::Enum(Enum {
                variants: Vec::new(),
                attrs,
            })
        }
        syn::Data::Union(_) => unimplemented!("union"),
    };

    Type(TypeNode::DataStructure {
        name: Ident::from(input.ident),
        generics,
        data,
    })
}

fn tracker_to_program(tracker: Tracker) -> Program {
    Program {
        crates: tracker.crates.into_inner(),
        impls: tracker
            .impls
            .into_inner()
            .into_iter()
            .map(into_complete_impl)
            .collect(),
    }
}

fn into_complete_impl(imp: WipImpl) -> (CompleteImpl, Option<TraitInferenceResult>) {
    let mut complete_impl = CompleteImpl {
        trait_ty: imp.trait_ty,
        ty: imp.ty,
        functions: imp
            .functions
            .into_inner()
            .into_iter()
            .map(|function| {
                let function: WipFunction = function;
                let values: Option<_> = function.values.into();
                let invokes: Option<_> = function.invokes.into();
                let macros: Option<_> = function.macros.into();
                CompleteFunction {
                    self_ty: function.self_ty,
                    f: function.f,
                    values: values.unwrap(),
                    invokes: invokes.unwrap(),
                    macros: macros.unwrap(),
                    ret: function.ret,
                }
            })
            .collect(),
    };
    let result = if complete_impl.has_generics() {
        Some(complete_impl.compute_trait_bounds())
    } else {
        None
    };
    (complete_impl, result)
}
