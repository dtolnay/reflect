use crate::ident::Ident;
use crate::{Function, Invoke, MacroInvoke, Print, Receiver, Type, TypeNode, ValueNode, ValueRef};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use ref_cast::RefCast;
use std::collections::BTreeSet as Set;

#[derive(Debug)]
pub(crate) struct Program {
    pub crates: Vec<Ident>,
    pub impls: Vec<CompleteImpl>,
}

#[derive(Debug)]
pub(crate) struct CompleteImpl {
    pub trait_ty: Option<Type>,
    pub ty: Type,
    pub functions: Vec<CompleteFunction>,
}

#[derive(Debug)]
pub(crate) struct CompleteFunction {
    pub self_ty: Option<Type>,
    pub f: Function,
    pub values: Vec<ValueNode>,
    pub invokes: Vec<Invoke>,
    pub macros: Vec<MacroInvoke>,
    pub ret: Option<ValueRef>,
}

impl Program {
    pub fn compile(&self) -> TokenStream {
        let impls = self.impls.iter().map(CompleteImpl::compile);

        quote! {
            #(#impls)*
        }
    }
}

impl CompleteImpl {
    fn compile(&self) -> TokenStream {
        let functions = self.functions.iter().map(CompleteFunction::compile);

        let (name, params, constraints) = self.ty.name_and_generics();
        let params = if params.is_empty() {
            None
        } else {
            let params = params.iter().map(Print::ref_cast);
            Some(quote!(<#(#params),*>))
        };
        let where_clause = if constraints.is_empty() {
            None
        } else {
            let constraints = constraints.iter().map(Print::ref_cast);
            Some(quote!(where #(#constraints,)*))
        };

        if let Some(trait_ty) = &self.trait_ty {
            let trait_ty = Print::ref_cast(trait_ty);
            quote! {
                // FIXME: assosiated types
                // FIXME: trait generics
                impl #params #trait_ty for #name #params #where_clause {
                    #(#functions)*
                }
            }
        } else {
            quote! {
                impl #params #name #params #where_clause {
                    #(#functions)*
                }
            }
        }
    }
}

impl CompleteFunction {
    fn compile(&self) -> TokenStream {
        let name = Ident::new(&self.f.content.name);

        let mut inputs = Vec::new();
        inputs.extend(receiver_tokens(self.f.content.sig.receiver));
        for (i, input) in self.f.content.sig.inputs.iter().enumerate() {
            let binding = Ident::new(format!("__arg{}", i));
            let ty = Print::ref_cast(input);
            inputs.push(quote! {
                #binding : #ty
            });
        }

        let output = match &self.f.content.sig.output {
            Type(TypeNode::Tuple(types)) if types.is_empty() => None,
            other => {
                let ty = Print::ref_cast(other);
                Some(quote!(-> #ty))
            }
        };

        let reachable = self.compute_reachability();
        let mutable = self.compute_mutability();
        let values = self.refs().flat_map(|v| {
            // Don't create let bindings for string literals as they're will be inlined
            if let ValueNode::Str(_) = self.values[v.0] {
                return None;
            }

            let expr = self.compile_value(v);
            if reachable.contains(&v) {
                let let_mut = if mutable.contains(&v) {
                    quote!(let mut)
                } else {
                    quote!(let)
                };
                let binding = v.binding();
                Some(quote! {
                    #let_mut #binding = #expr;
                })
            } else if self.is_important(v) {
                Some(quote! {
                    let _ = #expr;
                })
            } else {
                None
            }
        });

        let ret = self.ret.map(ValueRef::binding);

        quote! {
            fn #name (#(#inputs),*) #output {
                #(#values)*
                #ret
            }
        }
    }

    fn refs(&self) -> impl Iterator<Item = ValueRef> {
        (0..self.values.len()).map(ValueRef)
    }

    fn compute_reachability(&self) -> Set<ValueRef> {
        let mut reachable = Set::new();
        let mut stack: Vec<_> = self.refs().filter(|v| self.is_important(*v)).collect();

        if let Some(ret) = self.ret {
            stack.push(ret);
            reachable.insert(ret);
        }

        use crate::ValueNode::*;
        while let Some(v) = stack.pop() {
            match &self.values[v.0] {
                Tuple(values) => {
                    for &v in values.iter() {
                        if reachable.insert(v) {
                            stack.extend(values);
                        }
                    }
                }
                Str(s) => {}
                Reference(v) | ReferenceMut(v) | Dereference(v) => {
                    if reachable.insert(*v) {
                        stack.push(*v);
                    }
                }
                Binding { name, .. } => {}
                Invoke(invoke) => {
                    for &v in &self.invokes[invoke.0].args {
                        if reachable.insert(v) {
                            stack.push(v);
                        }
                    }
                }
                MacroInvocation(invoke) => {
                    for &v in &self.macros[invoke.0].args {
                        if reachable.insert(v) {
                            stack.push(v);
                        }
                    }
                }
                Destructure { parent, .. } => {
                    if reachable.insert(*parent) {
                        stack.push(*parent);
                    }
                }
                DataStructure { .. } => unimplemented!(),
            }
        }

        reachable
    }

    fn compute_mutability(&self) -> Set<ValueRef> {
        let mut mutable = Set::new();

        for v in self.refs() {
            if let ValueNode::ReferenceMut(referent) = self.values[v.0] {
                mutable.insert(referent);
            }
        }

        mutable
    }

    fn is_important(&self, v: ValueRef) -> bool {
        if let ValueNode::Invoke(_) | ValueNode::MacroInvocation(_) = self.values[v.0] {
            return true;
        }
        false
    }

    fn compile_value(&self, v: ValueRef) -> TokenStream {
        match &self.values[v.0] {
            ValueNode::Tuple(values) => {
                let values = self.make_values_list(values);

                quote! {
                    ( #values )
                }
            }
            ValueNode::Str(s) => quote! { #s },
            ValueNode::Reference(v) => {
                let v = v.binding();
                quote! { &#v }
            }
            ValueNode::ReferenceMut(v) => {
                let v = v.binding();
                quote! { &mut #v }
            }
            ValueNode::Dereference(v) => {
                let v = v.binding();
                quote! { *#v }
            }
            ValueNode::Binding { name, .. } => quote! { #name },
            ValueNode::Invoke(invoke) => {
                let invoke = &self.invokes[invoke.0];
                let parent_type = match invoke.function.content.parent {
                    Some(ref parent) => {
                        let print = Print::ref_cast(&parent.ty);
                        Some(quote!(#print ::))
                    }
                    None => None,
                };
                let name = Ident::new(&invoke.function.content.name);
                let args = self.make_values_list(&invoke.args);

                quote! {
                    #parent_type #name ( #args )
                }
            }
            ValueNode::Destructure {
                parent, accessor, ..
            } => {
                let parent = parent.binding();
                let accessor = Print::ref_cast(accessor);

                quote! {
                    &#parent.#accessor
                }
            }
            ValueNode::DataStructure { .. } => unimplemented!(),
            ValueNode::MacroInvocation(invoke) => {
                let invoke = &self.macros[invoke.0];
                let path = Print::ref_cast(&invoke.macro_path);
                let args = self.make_values_list(&invoke.args);

                let tokens = quote! {
                    #path ! ( #args )
                };

                tokens
            }
        }
    }

    /// Makes a list of comma-separated values with string literals inlined
    fn make_values_list(&self, values: &[ValueRef]) -> TokenStream {
        let values = values.iter().map(|value| match &self.values[value.0] {
            ValueNode::Str(s) => self.compile_value(*value),
            _ => value.binding().to_token_stream(),
        });

        quote! { #(#values),* }
    }
}

fn receiver_tokens(receiver: Receiver) -> Option<TokenStream> {
    match receiver {
        Receiver::NoSelf => None,
        Receiver::SelfByValue => Some(quote!(self)),
        Receiver::SelfByReference => Some(quote!(&self)),
        Receiver::SelfByReferenceMut => Some(quote!(&mut self)),
    }
}

impl ValueRef {
    fn binding(self) -> Ident {
        Ident::new(format!("__v{}", self.0))
    }
}
