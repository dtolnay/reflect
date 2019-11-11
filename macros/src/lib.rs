#![recursion_limit = "128"]

extern crate proc_macro;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{braced, parenthesized, parse_macro_input, token, Ident, Token};

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
    name: Ident,
    items: Vec<Item>,
}

struct ItemType {
    name: Ident,
}

struct ItemImpl {
    name: Ident,
    functions: Vec<Function>,
}

struct ItemTrait {
    name: Ident,
    functions: Vec<Function>,
}

struct Function {
    name: Ident,
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
    Ident(Ident),
    TraitObject(Vec<Ident>),
    Reference(Box<Type>),
    ReferenceMut(Box<Type>),
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
            let items = ItemMod::parse_items(input)?;
            crates.push(ItemMod { name, items });
        }
        Ok(Input { crates })
    }
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![mod]) {
            input.parse().map(Item::Mod)
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
    fn parse_items(input: ParseStream) -> Result<Vec<Item>> {
        let content;
        braced!(content in input);
        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }
        Ok(items)
    }
}

impl Parse for ItemMod {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![mod]>()?;
        let name: Ident = input.parse()?;
        let items = ItemMod::parse_items(input)?;
        Ok(ItemMod { name, items })
    }
}

impl Parse for ItemType {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![type]>()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(ItemType { name })
    }
}

impl Parse for ItemImpl {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![impl]>()?;
        let name: Ident = input.parse()?;

        let content;
        braced!(content in input);
        let mut functions = Vec::new();
        while !content.is_empty() {
            functions.push(content.parse()?);
        }

        Ok(ItemImpl { name, functions })
    }
}

impl Parse for ItemTrait {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![trait]>()?;
        let name: Ident = input.parse()?;

        let content;
        braced!(content in input);
        let mut functions = Vec::new();
        while !content.is_empty() {
            functions.push(content.parse()?);
        }

        Ok(ItemTrait { name, functions })
    }
}

impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![fn]>()?;
        let name: Ident = input.parse()?;

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

        input.parse::<Token![;]>()?;

        Ok(Function {
            name,
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
            let _: Token![dyn] = input.parse()?;
            let bounds: Punctuated<Ident, Token![+]> = Punctuated::parse_terminated(&input)?;
            Ok(Type::TraitObject(bounds.into_iter().collect()))
        } else if lookahead.peek(Ident) {
            input.parse().map(Type::Ident)
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
    let name = &module.name;
    let name_str = name.to_string();
    let items = module.items.iter().map(declare_item);

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

fn declare_item(item: &Item) -> TokenStream2 {
    match item {
        Item::Mod(item) => declare_mod(item),
        Item::Type(item) => declare_type(item),
        Item::Impl(item) => declare_impl(item),
        Item::Trait(item) => declare_trait(item),
        Item::Macro(item) => declare_macro(item),
    }
}

fn declare_type(item: &ItemType) -> TokenStream2 {
    let name = &item.name;
    let name_str = name.to_string();

    quote! {
        #[derive(Copy, Clone)]
        #[allow(non_camel_case_types)]
        pub struct #name;

        impl _reflect::runtime::RuntimeType for #name {
            fn SELF(self) -> _reflect::Type {
                MODULE().get_type(#name_str)
            }
        }
    }
}

fn declare_impl(item: &ItemImpl) -> TokenStream2 {
    let parent = &item.name;
    let functions = item.functions.iter().map(|f| declare_function(parent, f));

    quote! {
        #(
            #functions
        )*
    }
}

fn declare_trait(item: &ItemTrait) -> TokenStream2 {
    let d_type = declare_type(&ItemType {
        name: item.name.clone(),
    });
    let name = &item.name;
    let name_str = name.to_string();

    let parent = &item.name;
    let functions = item.functions.iter().map(|f| declare_function(parent, f));

    quote! {
        #d_type

        impl _reflect::runtime::RuntimeTrait for #name {
            fn SELF(self) -> _reflect::Path {
                MODULE().get_path(#name_str)
            }
        }
        #(
            #functions
        )*
    }
}

fn declare_function(parent: &Ident, function: &Function) -> TokenStream2 {
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
    let setup_inputs = function.args.iter().map(|arg| {
        let ty = to_runtime_type(arg);
        quote! {
            sig.add_input(#ty);
        }
    });
    let set_output = function.ret.as_ref().map(|ty| {
        let ty = to_runtime_type(&ty);
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
                    fn SELF(self) -> _reflect::Function {
                        let mut sig = _reflect::Signature::new();
                        #setup_receiver
                        #(
                            #setup_inputs
                        )*
                        #set_output
                        _reflect::runtime::RuntimeType::SELF(#parent).get_function(#name_str, sig)
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
                _reflect::Module::root().invoke_macro(#macro_name, values)
            }
        }
    }
}

fn to_runtime_type(ty: &Type) -> TokenStream2 {
    match ty {
        Type::Tuple(types) => {
            let types = types.iter().map(to_runtime_type);
            quote! {
                _reflect::Type::tuple(&[#(#types),*])
            }
        }
        Type::Ident(ident) => quote! {
            _reflect::runtime::RuntimeType::SELF(#ident)
        },
        Type::TraitObject(bounds) => {
            quote! {
                _reflect::runtime::RuntimeTraitObject::SELF(&[
                    #(_reflect::runtime::RuntimeTrait::SELF(#bounds)),*
                ] as &[_])
            }
        }
        Type::Reference(inner) => {
            let inner = to_runtime_type(inner);
            quote! {
                #inner.reference()
            }
        }
        Type::ReferenceMut(inner) => {
            let inner = to_runtime_type(inner);
            quote! {
                #inner.reference_mut()
            }
        }
    }
}
