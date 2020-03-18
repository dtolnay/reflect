#![recursion_limit = "128"]

extern crate proc_macro;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use std::iter::once;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{
    braced, parenthesized, parse_macro_input, parse_str, token, GenericArgument, GenericParam,
    Generics, Ident, Lifetime, Path, PathArguments, PathSegment, ReturnType, Token, TypeParamBound,
    TypeTraitObject,
};

use self::proc_macro::TokenStream;

struct Input {
    crates: Vec<ItemMod>,
}

enum Item {
    Mod(ItemMod),
    Type(ItemType),
    Impl(ItemImpl),
    Trait(ItemTrait),
    Macro(ItemMacro),
}

struct ItemMod {
    path: Path,
    items: Vec<Item>,
}

struct ItemType {
    segment: PathSegment,
}

struct ItemImpl {
    segment: PathSegment,
    generics: Generics,
    functions: Vec<Function>,
}

struct ItemTrait {
    segment: PathSegment,
    generics: Generics,
    functions: Vec<Function>,
}

struct Function {
    name: Ident,
    generics: Generics,
    receiver: Receiver,
    args: Vec<Type>,
    ret: Option<Type>,
}

struct ItemMacro {
    name: Ident,
}

enum Receiver {
    None,
    ByValue,
    ByRef,
    ByMut,
}

enum Type {
    Tuple(Vec<Type>),
    Path(Path),
    TraitObject(TypeTraitObject),
    Reference(Box<Type>),
    ReferenceMut(Box<Type>),
}

enum ParentKind {
    Trait,
    DataStructure,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut crates = Vec::new();
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![use]) {
                input.parse::<Token![use]>()?;
            } else {
                input.parse::<Token![extern]>()?;
                input.parse::<Token![crate]>()?;
            }
            let name: Ident = input.parse()?;
            let mut segments = Punctuated::new();
            segments.push(PathSegment {
                ident: name.clone(),
                arguments: PathArguments::None,
            });
            let path = Path {
                leading_colon: Some(Token![::](Span::call_site())),
                segments,
            };
            let items = ItemMod::parse_items(input, &path.clone())?;
            crates.push(ItemMod { path, items });
        }
        Ok(Input { crates })
    }
}

impl Item {
    fn parse(input: ParseStream, mod_path: &Path) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![mod]) {
            ItemMod::parse(&input, mod_path).map(Item::Mod)
        } else if lookahead.peek(Token![type]) {
            input.parse().map(Item::Type)
        } else if lookahead.peek(Token![impl]) {
            input.parse().map(Item::Impl)
        } else if lookahead.peek(Token![trait]) {
            input.parse().map(Item::Trait)
        } else if lookahead.peek(Token![macro]) {
            input.parse().map(Item::Macro)
        } else {
            Err(lookahead.error())
        }
    }
}

impl ItemMod {
    fn parse_items(input: ParseStream, mod_path: &Path) -> Result<Vec<Item>> {
        let content;
        braced!(content in input);
        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(Item::parse(&content, mod_path)?);
        }
        Ok(items)
    }
}

impl ItemMod {
    fn parse(input: ParseStream, mod_path: &Path) -> Result<Self> {
        input.parse::<Token![mod]>()?;
        let name: Ident = input.parse()?;
        let mut path = mod_path.clone();
        path.segments.push(PathSegment {
            ident: name.clone(),
            arguments: PathArguments::None,
        });
        let items = ItemMod::parse_items(input, &path.clone())?;
        Ok(ItemMod { path, items })
    }
}

impl Parse for ItemType {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![type]>()?;
        let segment = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(ItemType { segment })
    }
}

fn has_generics(input: ParseStream) -> bool {
    input.peek(Token![<])
        && (input.peek2(Token![>])
            || input.peek2(Token![#])
            || (input.peek2(Ident) || input.peek2(Lifetime))
                && (input.peek3(Token![:]) || input.peek3(Token![,]) || input.peek3(Token![>])))
}

impl Parse for ItemImpl {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![impl]>()?;

        let mut generics = if has_generics(input) {
            input.parse()?
        } else {
            Generics::default()
        };
        let segment = input.parse()?;
        generics.where_clause = input.parse()?;

        let content;
        braced!(content in input);
        let mut functions = Vec::new();
        while !content.is_empty() {
            functions.push(content.parse()?);
        }

        Ok(ItemImpl {
            segment,
            generics,
            functions,
        })
    }
}

impl Parse for ItemTrait {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![trait]>()?;

        let mut generics: Generics = Generics::default();
        let segment: PathSegment = input.parse()?;

        if let PathArguments::AngleBracketed(args) = &segment.arguments {
            let args = args
                .args
                .iter()
                .filter_map::<GenericParam, _>(|arg| match arg {
                    GenericArgument::Type(syn::Type::Path(path)) => {
                        if let Some(ident) = path.path.get_ident() {
                            parse_str(&ident.to_string()).ok()
                        } else {
                            None
                        }
                    }
                    GenericArgument::Constraint(constraint) => {
                        parse_str(&constraint.ident.to_string()).ok()
                    }
                    _ => None,
                })
                .collect();
            generics.params = args;
        };
        generics.where_clause = input.parse()?;

        let content;
        braced!(content in input);
        let mut functions = Vec::new();
        while !content.is_empty() {
            functions.push(content.parse()?);
        }

        Ok(ItemTrait {
            segment,
            generics,
            functions,
        })
    }
}

impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![fn]>()?;
        let name: Ident = input.parse()?;

        let mut generics = if has_generics(input) {
            input.parse()?
        } else {
            Generics::default()
        };

        let argument_list;
        parenthesized!(argument_list in input);

        let receiver: Receiver = argument_list.parse()?;
        if !(receiver.is_none() || argument_list.is_empty()) {
            argument_list.parse::<Token![,]>()?;
        }

        let mut args = Vec::new();
        while !argument_list.is_empty() {
            args.push(argument_list.parse()?);
            if argument_list.is_empty() {
                break;
            }
            argument_list.parse::<Token![,]>()?;
        }

        let ret = if input.peek(Token![->]) {
            input.parse::<Token![->]>()?;
            Some(input.parse()?)
        } else {
            None
        };

        generics.where_clause = input.parse()?;

        input.parse::<Token![;]>()?;

        Ok(Function {
            name,
            generics,
            receiver,
            args,
            ret,
        })
    }
}

impl Parse for Receiver {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![self]) {
            input.parse::<Token![self]>()?;
            Ok(Receiver::ByValue)
        } else if input.peek(Token![&]) && input.peek2(Token![self]) {
            input.parse::<Token![&]>()?;
            input.parse::<Token![self]>()?;
            Ok(Receiver::ByRef)
        } else if input.peek(Token![&]) && input.peek2(Token![mut]) && input.peek3(Token![self]) {
            input.parse::<Token![&]>()?;
            input.parse::<Token![mut]>()?;
            input.parse::<Token![self]>()?;
            Ok(Receiver::ByMut)
        } else {
            Ok(Receiver::None)
        }
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let content: Punctuated<Type, Token![,]> = Punctuated::parse_terminated(&content)?;
            if content.len() == 1 && !content.trailing_punct() {
                // It is not a tuple. The parentheses were just used to
                // disambiguate the type.
                Ok(content.into_iter().next().unwrap())
            } else {
                Ok(Type::Tuple(content.into_iter().collect()))
            }
        } else if lookahead.peek(Token![&]) {
            input.parse::<Token![&]>()?;
            let mut_token: Option<Token![mut]> = input.parse()?;
            let inner: Type = input.parse()?;
            if mut_token.is_some() {
                Ok(Type::ReferenceMut(Box::new(inner)))
            } else {
                Ok(Type::Reference(Box::new(inner)))
            }
        } else if lookahead.peek(Token![dyn]) {
            Ok(Type::TraitObject(input.parse()?))
        } else if lookahead.peek(Ident) || lookahead.peek(Token![::]) {
            input.parse().map(Type::Path)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ItemMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![macro]>()?;

        let name: Ident = input.parse()?;
        input.parse::<Token![;]>()?;

        Ok(Self { name })
    }
}

impl Receiver {
    fn is_none(&self) -> bool {
        use self::Receiver::*;
        match self {
            None => true,
            ByValue | ByRef | ByMut => false,
        }
    }
}

#[proc_macro]
pub fn library(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);

    let modules = input.crates.iter().map(declare_mod);

    TokenStream::from(quote! {
        #[allow(non_snake_case)]
        mod RUNTIME {
            extern crate reflect as _reflect;

            #[allow(dead_code, non_snake_case)]
            pub fn MODULE() -> _reflect::Module {
                _reflect::Module::root()
            }

            #(
                #modules
            )*
        }
    })
}

fn declare_mod(module: &ItemMod) -> TokenStream2 {
    let path = &module.path;
    let name = path.segments.last();
    let name_str = name.map(|segment| segment.ident.to_string());
    let items = module
        .items
        .iter()
        .map(|item| declare_item(item, &module.path));

    quote! {
        pub mod #name {
            extern crate reflect as _reflect;

            #[allow(unused_imports)]
            use self::_reflect::runtime::prelude::*;

            #[allow(dead_code, non_snake_case)]
            pub fn MODULE() -> _reflect::Module {
                super::MODULE().get_module(#name_str)
            }

            struct __Indirect<T>(T);

            #(
                #items
            )*
        }
    }
}

fn declare_item(item: &Item, mod_path: &Path) -> TokenStream2 {
    match item {
        Item::Mod(item) => declare_mod(item),
        Item::Type(item) => declare_type(item),
        Item::Impl(item) => declare_impl(item, mod_path),
        Item::Trait(item) => declare_trait(item, mod_path),
        Item::Macro(item) => declare_macro(item),
    }
}

fn declare_type(item: &ItemType) -> TokenStream2 {
    let name = &item.segment.ident;

    quote! {
        #[derive(Copy, Clone)]
        #[allow(non_camel_case_types)]
        pub struct #name;
    }
}

fn declare_parent(
    parent_generics: &Generics,
    parent_type: &PathSegment,
    mod_path: &Path,
    type_params: &Vec<&Ident>,
    parent_kind: ParentKind,
) -> TokenStream2 {
    let set_parent_params = if !parent_generics.params.is_empty() {
        let param_strings = parent_generics
            .params
            .iter()
            .map(|param| param.to_token_stream().to_string());

        Some(quote! {
            let mut generics: _reflect::Generics = ::std::default::Default::default();
            let mut param_map = generics.set_generic_params(&[#(#param_strings),*]);
        })
    } else {
        None
    };

    let set_parent_constraints = if let Some(clause) = &parent_generics.where_clause {
        let constraint_strings = clause
            .predicates
            .iter()
            .map(|clause| clause.to_token_stream().to_string());

        Some(quote! {
            generics.set_generic_constraints(&[#(#constraint_strings),*], &mut param_map);
        })
    } else {
        None
    };

    let parent = &parent_type.ident;
    let parent_kind = match parent_kind {
        ParentKind::Trait => quote! {
            _reflect::ParentKind::Trait
        },
        ParentKind::DataStructure => quote! {
            _reflect::ParentKind::DataStructure
        },
    };

    let mut segments = Punctuated::new();
    segments.push(parent_type.clone());
    let get_runtime_path = to_runtime_path(
        &Path {
            leading_colon: None,
            segments,
        },
        mod_path,
        type_params,
    );

    let set_generics = if !parent_generics.params.is_empty() {
        Some(quote! {
            parent.set_generics(generics);
        })
    } else {
        None
    };

    quote! {
        impl _reflect::runtime::RuntimeParent for #parent {
            fn SELF(self) -> ::std::rc::Rc<_reflect::Parent> {
                thread_local! {
                    static PARENT: ::std::rc::Rc<_reflect::Parent> = {
                        #set_parent_params
                        #set_parent_constraints
                        let path = #get_runtime_path;
                        let mut parent = _reflect::Parent::new(path, #parent_kind);
                        #set_generics
                        ::std::rc::Rc::new(parent)
                    };
                }
                PARENT.with(::std::rc::Rc::clone)
            }
        }
    }
}

fn declare_impl(item: &ItemImpl, mod_path: &Path) -> TokenStream2 {
    let parent = &item.segment.ident;
    let type_params = &item
        .generics
        .params
        .iter()
        .filter_map(|param| {
            if let GenericParam::Type(type_param) = param {
                Some(&type_param.ident)
            } else {
                None
            }
        })
        .collect();

    let declare_parent = declare_parent(
        &item.generics,
        &item.segment,
        mod_path,
        type_params,
        ParentKind::DataStructure,
    );

    let functions = item.functions.iter().map(|f| {
        declare_function(
            parent,
            !item.generics.params.is_empty(),
            f,
            mod_path,
            type_params,
        )
    });

    quote! {
        #declare_parent
        impl _reflect::runtime::RuntimeImpl for #parent {}
        #(
            #functions
        )*
    }
}

fn declare_trait(item: &ItemTrait, mod_path: &Path) -> TokenStream2 {
    let d_type = declare_type(&ItemType {
        segment: item.segment.clone(),
    });
    let parent = &item.segment.ident;

    let type_params = &item
        .generics
        .params
        .iter()
        .filter_map(|param| {
            if let GenericParam::Type(type_param) = param {
                Some(&type_param.ident)
            } else {
                None
            }
        })
        .collect();

    let declare_parent = declare_parent(
        &item.generics,
        &item.segment,
        mod_path,
        type_params,
        ParentKind::Trait,
    );

    let functions = item.functions.iter().map(|f| {
        declare_function(
            parent,
            !item.generics.params.is_empty(),
            f,
            mod_path,
            type_params,
        )
    });

    quote! {
        #d_type
        #declare_parent
        impl _reflect::runtime::RuntimeTrait for #parent {}
        #(
            #functions
        )*
    }
}

fn declare_function(
    parent: &Ident,
    parent_has_generics: bool,
    function: &Function,
    mod_path: &Path,
    type_params: &Vec<&Ident>,
) -> TokenStream2 {
    let name = &function.name;
    let name_str = name.to_string();
    let setup_receiver = match function.receiver {
        Receiver::None => None,
        Receiver::ByValue => Some(quote! {
            sig.set_self_by_value();
        }),
        Receiver::ByRef => Some(quote! {
            sig.set_self_by_reference();
        }),
        Receiver::ByMut => Some(quote! {
            sig.set_self_by_reference_mut();
        }),
    };
    let type_params = &function
        .generics
        .params
        .iter()
        .filter_map(|param| {
            if let GenericParam::Type(type_param) = param {
                Some(&type_param.ident)
            } else {
                None
            }
        })
        .chain(type_params.iter().map(|ident| *ident))
        .collect();

    let get_parent_param_map = if parent_has_generics {
        Some(quote! {
            let mut param_map = parent.get_param_map().unwrap();
        })
    } else {
        None
    };

    let set_sig_params = if !function.generics.params.is_empty() {
        let param_strings = function
            .generics
            .params
            .iter()
            .map(|param| param.to_token_stream().to_string());

        if parent_has_generics {
            Some(quote! {
                param_map.append(
                    &mut sig.set_generic_params(&[#(#param_strings),*])
                );
            })
        } else {
            Some(quote! {
                let mut param_map = sig.set_generic_params(&[#(#param_strings),*]);
            })
        }
    } else {
        None
    };

    let set_sig_constraints = if let Some(clause) = &function.generics.where_clause {
        let constraint_strings = clause
            .predicates
            .iter()
            .map(|clause| clause.to_token_stream().to_string());

        Some(quote! {
            sig.set_generic_constraints(&[#(#constraint_strings),*], &mut param_map);
        })
    } else {
        None
    };

    let setup_inputs = function.args.iter().map(|arg| {
        let ty = to_runtime_type(arg, mod_path, type_params);
        quote!(sig.add_input(#ty);)
    });
    let set_output = function.ret.as_ref().map(|ty| {
        let ty = to_runtime_type(&ty, mod_path, type_params);
        quote!(sig.set_output(#ty);)
    });

    let vars = (0..(!function.receiver.is_none() as usize + function.args.len()))
        .map(|i| Ident::new(&format!("v{}", i), Span::call_site()));
    let vars2 = vars.clone();

    quote! {
        impl __Indirect<#parent> {
            #[allow(dead_code)]
            fn #name() {
                #[allow(non_camel_case_types)]
                #[derive(Copy, Clone)]
                pub struct #name;

                impl _reflect::runtime::RuntimeFunction for #name {
                    fn SELF(self) -> ::std::rc::Rc<_reflect::Function> {
                        thread_local! {
                            static FUNCTION: ::std::rc::Rc<_reflect::Function> = {
                                let mut sig = _reflect::Signature::new();
                                let parent = _reflect::runtime::RuntimeParent::SELF(#parent);
                                #get_parent_param_map
                                #set_sig_params
                                #set_sig_constraints
                                #setup_receiver
                                #(
                                    #setup_inputs
                                )*
                                #set_output
                                let mut fun = _reflect::Function::get_function(#name_str, sig);
                                fun.set_parent(parent);
                                ::std::rc::Rc::new(fun)
                            };
                        };
                        FUNCTION.with(::std::rc::Rc::clone)
                    }
                }

                impl #name {
                    pub fn INVOKE(
                        self,
                        #(
                            #vars: _reflect::Value,
                        )*
                    ) -> _reflect::Value {
                        _reflect::runtime::RuntimeFunction::SELF(self).invoke(&[#(#vars2),*])
                    }
                }

                impl #parent {
                    #[allow(non_upper_case_globals)]
                    pub const #name: #name = #name;
                }
            }
        }
    }
}

fn declare_macro(item: &ItemMacro) -> TokenStream2 {
    let name = &item.name;
    let macro_name = name.to_string();

    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone)]
        pub struct #name;

        impl #name {
            #[allow(non_upper_case_globals)]
            pub const #name: #name = #name;

            pub fn INVOKE(
                self,
                values: &[_reflect::Value],
            ) -> _reflect::Value {
                MODULE().invoke_macro(#macro_name, values)
            }
        }
    }
}

fn to_runtime_type(ty: &Type, mod_path: &Path, type_params: &Vec<&Ident>) -> TokenStream2 {
    match ty {
        Type::Tuple(types) => {
            let types = types
                .iter()
                .map(|ty| to_runtime_type(ty, mod_path, type_params));
            quote! {
                _reflect::Type::tuple(&[#(#types),*])
            }
        }
        Type::Path(path) => {
            if let Some(ident) = &path.get_ident() {
                // Check if the path is a generic argument
                if type_params.contains(ident) {
                    let type_param = ident.to_string();
                    return quote! {
                        _reflect::Type::type_param_from_str(#type_param, &mut param_map)
                    };
                }
            }
            to_runtime_path_type(path, mod_path, type_params)
        }

        Type::TraitObject(type_trait_objects) => {
            let bound_strings = type_trait_objects
                .bounds
                .iter()
                .map(|bound| bound.to_token_stream().to_string());
            if type_trait_objects.bounds.iter().any(|bound| match bound {
                TypeParamBound::Lifetime(_) => true,
                TypeParamBound::Trait(trait_bound) => {
                    trait_bound.lifetimes.is_some()
                        || trait_bound
                            .path
                            .segments
                            .iter()
                            .any(|segment| !segment.arguments.is_empty())
                }
            }) {
                quote! {
                    _reflect::Type::get_trait_object(&[#(#bound_strings),*], &mut param_map)
                }
            } else {
                quote! {
                    _reflect::Type::get_simple_trait_object(&[#(#bound_strings),*])
                }
            }
        }
        Type::Reference(inner) => {
            let inner = to_runtime_type(inner, mod_path, type_params);
            quote! {
                #inner.reference()
            }
        }
        Type::ReferenceMut(inner) => {
            let inner = to_runtime_type(inner, mod_path, type_params);
            quote! {
                #inner.reference_mut()
            }
        }
    }
}

fn to_runtime_path_type(path: &Path, mod_path: &Path, type_params: &Vec<&Ident>) -> TokenStream2 {
    let path = to_runtime_path(path, mod_path, type_params);
    quote! {
        _reflect::runtime::RuntimeType::SELF(
            #path
        )
    }
}

fn to_runtime_path(path: &Path, mod_path: &Path, type_params: &Vec<&Ident>) -> TokenStream2 {
    let mut path = path.clone();

    // Check if path is defined in current module
    if path.segments.len() == 1 && path.leading_colon.is_none() {
        let mut full_path = mod_path.clone();
        full_path.segments.push(path.segments[0].clone());
        full_path.to_token_stream().to_string();
        path = full_path
    }

    if path.segments.last().unwrap().arguments.is_empty() {
        let path_str = path.to_token_stream().to_string();
        quote! {
            _reflect::Path::simple_path_from_str(#path_str)
        }
    } else {
        let arguments = &mut path.segments.last_mut().unwrap().arguments;
        expand_path_arguments(arguments, mod_path, type_params);
        let path_str = path.to_token_stream().to_string();
        quote! {
            _reflect::Path::path_from_str(#path_str, &mut param_map)
        }
    }
}

/// Expand module defined types inside of a PathArgumet as fully qualified paths
fn expand_path_arguments(
    arguments: &mut PathArguments,
    mod_path: &Path,
    type_params: &Vec<&Ident>,
) {
    match arguments {
        PathArguments::None => {}
        PathArguments::AngleBracketed(generic_args) => {
            generic_args.args.iter_mut().for_each(|arg| match arg {
                GenericArgument::Type(ty) => expand_type(ty, mod_path, type_params),

                GenericArgument::Binding(binding) => {
                    expand_type(&mut binding.ty, mod_path, type_params)
                }

                GenericArgument::Constraint(constraint) => {
                    constraint.bounds.iter_mut().for_each(|bound| {
                        if let TypeParamBound::Trait(bound) = bound {
                            expand_path(&mut bound.path, mod_path, type_params)
                        }
                    })
                }
                GenericArgument::Const(_expr) => unimplemented!(),
                GenericArgument::Lifetime(_) => {}
            })
        }
        PathArguments::Parenthesized(generic_args) => {
            generic_args
                .inputs
                .iter_mut()
                .for_each(|ty| expand_type(ty, mod_path, type_params));

            if let ReturnType::Type(_, ref mut ty) = generic_args.output {
                expand_type(ty, mod_path, type_params)
            }
        }
    }
}

/// Expand module defined types inside of the PathArguments inside of a type
fn expand_type(ty: &mut syn::Type, mod_path: &Path, type_params: &Vec<&Ident>) {
    use syn::Type::*;
    match ty {
        Path(type_path) => expand_path(&mut type_path.path, mod_path, type_params),
        Reference(reference) => expand_type(&mut reference.elem, mod_path, type_params),

        TraitObject(type_trait_object) => type_trait_object.bounds.iter_mut().for_each(|bound| {
            if let TypeParamBound::Trait(bound) = bound {
                expand_path(&mut bound.path, mod_path, type_params)
            }
        }),

        Tuple(type_tuple) => type_tuple
            .elems
            .iter_mut()
            .for_each(|elem| expand_type(elem, mod_path, type_params)),

        // TODO: maybe return syn::Error?
        _ => unimplemented!("expand_type_arguments: Tried to expand unsupported type"),
    }
}

/// If path is an type defined in the current module, make the fully qualified
/// type for that path
fn expand_path(path: &mut syn::Path, mod_path: &Path, type_params: &Vec<&Ident>) {
    // Expand path arguments if any
    let path_arguments = &mut path.segments.last_mut().unwrap().arguments;
    expand_path_arguments(path_arguments, mod_path, type_params);

    // If the type is defined in the current scope, expand to the fully quallified path
    // FIXME: This test assumes that a type with one path segment and no leading
    // colon is defined in the current scope. This may not be the case if the macro
    // is not defined correctly.
    if path.segments.len() == 1 && path.leading_colon.is_none() {
        let segment = &path.segments[0];
        if !type_params.contains(&&segment.ident) {
            let mut segments = Punctuated::new();
            mod_path
                .segments
                .iter()
                .cloned()
                .chain(once(segment.clone()))
                .for_each(|segment| segments.push(segment));
            path.segments = segments;
            path.leading_colon = Some(Token![::](Span::call_site()));
        }
    }
}
