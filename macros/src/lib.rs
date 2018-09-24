extern crate proc_macro;

use quote::quote;
use syn::{braced, parenthesized, parse_macro_input, token, Ident, Token};
use syn::parse::{Parse, ParseStream, Result};

use self::proc_macro::TokenStream;

struct Input {
    crates: Vec<ItemMod>,
}

enum Item {
    Mod(ItemMod),
    Type(ItemType),
    Impl(ItemImpl),
    Trait(ItemTrait),
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

enum Receiver {
    None,
    ByValue,
    ByRef,
    ByMut,
}

enum Type {
    Unit,
    Ident(Ident),
    Reference(Box<Type>),
    ReferenceMut(Box<Type>),
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut crates = Vec::new();
        while !input.is_empty() {
            input.parse::<Token![extern]>()?;
            input.parse::<Token![crate]>()?;
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

        Ok(Function { name, receiver, args, ret })
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
            let _ = content;
            Ok(Type::Unit)
        } else if lookahead.peek(Token![&]) {
            input.parse::<Token![&]>()?;
            let mut_token: Option<Token![mut]> = input.parse()?;
            let inner: Type = input.parse()?;
            if mut_token.is_some() {
                Ok(Type::ReferenceMut(Box::new(inner)))
            } else {
                Ok(Type::Reference(Box::new(inner)))
            }
        } else if lookahead.peek(Ident) {
            input.parse().map(Type::Ident)
        } else {
            Err(lookahead.error())
        }
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

    unimplemented!()
}
