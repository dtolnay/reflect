use crate::ident::Ident;
use crate::{
    Function, GlobalBorrow, InvokeRef, MacroInvokeRef, Parent, PathArguments, Print, Receiver,
    SimplePath, TraitInferenceResult, Type, TypeNode, ValueNode, ValueRef, INVOKES, MACROS, VALUES,
};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use ref_cast::RefCast;
use std::collections::BTreeSet as Set;
use std::ops::Range;
use std::rc::Rc;
use syn::Token;

#[derive(Debug)]
pub(crate) struct Program {
    pub crates: Vec<Ident>,
    pub impls: Vec<CompleteImpl>,
}

#[derive(Debug)]
pub(crate) struct CompleteImpl {
    pub trait_ty: Option<Rc<Parent>>,
    pub ty: Type,
    pub functions: Vec<CompleteFunction>,
    pub result: Option<TraitInferenceResult>,
}

#[derive(Debug)]
pub(crate) struct CompleteFunction {
    pub self_ty: Option<Type>,
    pub f: Rc<Function>,
    pub values: Range<ValueRef>,
    pub invokes: Range<InvokeRef>,
    pub macros: Range<MacroInvokeRef>,
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

        let name = if let TypeNode::DataStructure(data) = &self.ty.0 {
            &data.name
        } else {
            panic!()
        };
        let (params, self_ty_args, where_clause, trait_ty) = if let Some(result) = &self.result {
            let params = result.generic_params.iter().map(Print::ref_cast);
            let params = Some(quote!(<#(#params),*>));
            let constraints = result.constraints.set.iter().map(Print::ref_cast);
            let self_ty_args = result.data_struct_args.args.iter().map(Print::ref_cast);
            let self_ty_args = if result.data_struct_args.args.is_empty() {
                None
            } else {
                Some(quote!(<#(#self_ty_args),*>))
            };
            let where_clause = if result.constraints.set.is_empty() {
                None
            } else {
                Some(quote!(where #(#constraints,)*))
            };
            let trait_ty = self.trait_ty.as_ref().map(|parent| {
                let mut path = parent.path.clone();
                let args = &mut path.path.last_mut().unwrap().args;
                if let PathArguments::AngleBracketed(args) = args {
                    args.args = result.trait_args.clone();
                };
                let path = Print::ref_cast(&path);
                quote!(#path)
            });
            (params, self_ty_args, where_clause, trait_ty)
        } else {
            let trait_ty = self.trait_ty.as_ref().map(|trait_ty| {
                let path = Print::ref_cast(&trait_ty.path);
                quote!(#path)
            });
            (None, None, None, trait_ty)
        };

        if let Some(trait_ty) = trait_ty {
            quote! {
                // FIXME: assosiated types
                impl #params #trait_ty for #name #self_ty_args #where_clause {
                    #(#functions)*
                }
            }
        } else {
            quote! {
                impl #params #name #self_ty_args #where_clause {
                    #(#functions)*
                }
            }
        }
    }
}

impl CompleteFunction {
    fn compile(&self) -> TokenStream {
        let name = Ident::new(&self.f.name);

        let generics = &self.f.sig.generics;
        let (params, where_clause) = if !generics.params.is_empty() {
            let params = generics.params.iter().map(Print::ref_cast);
            let params = Some(quote!(<#(#params),*>));
            let where_clause = if generics.constraints.is_empty() {
                None
            } else {
                let constraints = generics.constraints.iter().map(Print::ref_cast);
                Some(quote!(where #(#constraints,)*))
            };
            (params, where_clause)
        } else {
            (None, None)
        };

        let mut inputs = Vec::new();
        inputs.extend(receiver_tokens(self.f.sig.receiver));
        for (i, input) in self.f.sig.inputs.iter().enumerate() {
            let binding = Ident::new(format!("__arg{}", i));
            let ty = Print::ref_cast(input);
            inputs.push(quote! {
                #binding : #ty
            });
        }

        let output = match &self.f.sig.output {
            Type(TypeNode::Tuple(types)) if types.is_empty() => None,
            other => {
                let ty = Print::ref_cast(other);
                Some(quote!(-> #ty))
            }
        };

        let reachable = self.compute_reachability();
        let mutable = self.compute_mutability();
        let values = self.refs().flat_map(|v| {
            VALUES.with_borrow(|values| {
                // Don't create let bindings for string literals as they're will be inlined
                if let ValueNode::Str(_) = values[v.0] {
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
            })
        });

        let ret = self.ret.map(ValueRef::binding);

        quote! {
            fn #name #params (#(#inputs),*) #output #where_clause {
                #(#values)*
                #ret
            }
        }
    }

    fn refs(&self) -> impl Iterator<Item = ValueRef> {
        (self.values.start.0..self.values.end.0).map(ValueRef)
    }

    fn compute_reachability(&self) -> Set<ValueRef> {
        use crate::ValueNode::*;

        let mut reachable = Set::new();
        let mut stack: Vec<_> = self.refs().filter(|v| self.is_important(*v)).collect();

        if let Some(ret) = self.ret {
            stack.push(ret);
            reachable.insert(ret);
        }

        while let Some(v) = stack.pop() {
            VALUES.with_borrow(|values| match &values[v.0] {
                Tuple(values) => {
                    for &v in values.iter() {
                        if reachable.insert(v) {
                            stack.extend(values);
                        }
                    }
                }
                Str(s) => {}
                Reference { value: v, .. } | Dereference(v) => {
                    if reachable.insert(*v) {
                        stack.push(*v);
                    }
                }
                Binding { name, .. } => {}
                Invoke(invoke) => INVOKES.with_borrow(|invokes| {
                    for &v in &invokes[invoke.0].args {
                        if reachable.insert(v) {
                            stack.push(v);
                        }
                    }
                }),
                MacroInvocation(invoke) => MACROS.with_borrow(|macros| {
                    for &v in &macros[invoke.0].args {
                        if reachable.insert(v) {
                            stack.push(v);
                        }
                    }
                }),
                Destructure { parent, .. } => {
                    if reachable.insert(*parent) {
                        stack.push(*parent);
                    }
                }
                DataStructure { .. } => unimplemented!(),
            })
        }

        reachable
    }

    fn compute_mutability(&self) -> Set<ValueRef> {
        let mut mutable = Set::new();

        for v in self.refs() {
            VALUES.with_borrow(|values| {
                if let ValueNode::Reference { is_mut, value } = values[v.0] {
                    if is_mut {
                        mutable.insert(value);
                    }
                }
            })
        }

        mutable
    }

    fn is_important(&self, v: ValueRef) -> bool {
        VALUES.with_borrow(|values| {
            if let ValueNode::Invoke(_) | ValueNode::MacroInvocation(_) = values[v.0] {
                return true;
            }
            false
        })
    }

    fn compile_value(&self, v: ValueRef) -> TokenStream {
        VALUES.with_borrow(|values| match &values[v.0] {
            ValueNode::Tuple(values) => {
                let values = self.make_values_list(values);

                quote! {
                    ( #values )
                }
            }
            ValueNode::Str(s) => quote! { #s },
            ValueNode::Reference { is_mut, value } if !is_mut => {
                let v = value.binding();
                quote! { &#v }
            }
            ValueNode::Reference { is_mut, value } => {
                let v = value.binding();
                quote! { &mut #v }
            }
            ValueNode::Dereference(v) => {
                let v = v.binding();
                quote! { *#v }
            }
            ValueNode::Binding { name, .. } => quote! { #name },
            ValueNode::Invoke(invoke) => INVOKES.with_borrow(|invokes| {
                let invoke = &invokes[invoke.0];
                let parent_type = match invoke.function.parent {
                    Some(ref parent) => {
                        let print = Print::ref_cast(SimplePath::ref_cast(&parent.path));
                        Some(quote!(#print ::))
                    }
                    None => None,
                };
                let name = Ident::new(&invoke.function.name);
                let args = self.make_values_list(&invoke.args);

                quote! {
                    #parent_type #name ( #args )
                }
            }),
            ValueNode::Destructure {
                parent,
                accessor,
                ty,
            } => {
                let mut node = &parent.node().get_type().0;
                let parent = parent.binding();
                let accessor = Print::ref_cast(accessor);
                let mut references = TokenStream::new();

                while let TypeNode::Reference { is_mut, inner, .. } = node {
                    Token!(&)(Span::call_site()).to_tokens(&mut references);
                    if *is_mut {
                        Token![mut](Span::call_site()).to_tokens(&mut references);
                    }
                    node = &**inner;
                }
                quote!(#references #parent.#accessor)
            }
            ValueNode::DataStructure { .. } => unimplemented!(),
            ValueNode::MacroInvocation(invoke) => MACROS.with_borrow(|macros| {
                let invoke = &macros[invoke.0];
                let path = Print::ref_cast(&invoke.macro_path);
                let args = self.make_values_list(&invoke.args);

                let tokens = quote! {
                    #path ! ( #args )
                };

                tokens
            }),
        })
    }

    /// Makes a list of comma-separated values with string literals inlined
    fn make_values_list(&self, values: &[ValueRef]) -> TokenStream {
        let values = values.iter().map(|value| {
            VALUES.with_borrow(|values| match &values[value.0] {
                ValueNode::Str(s) => self.compile_value(*value),
                _ => value.binding().to_token_stream(),
            })
        });

        quote! { #(#values),* }
    }
}

fn receiver_tokens(receiver: Receiver) -> Option<TokenStream> {
    match receiver {
        Receiver::NoSelf => None,
        Receiver::SelfByValue => Some(quote!(self)),
        Receiver::SelfByReference { is_mut, lifetime } if !is_mut => {
            let lifetime = lifetime
                .0
                .as_ref()
                .map(|lifetime| Print::ref_cast(lifetime));
            Some(quote!(&#lifetime self))
        }
        Receiver::SelfByReference { is_mut, lifetime } => {
            let lifetime = lifetime
                .0
                .as_ref()
                .map(|lifetime| Print::ref_cast(lifetime));
            Some(quote!(&#lifetime mut self))
        }
    }
}

impl ValueRef {
    fn binding(self) -> Ident {
        Ident::new(format!("__v{}", self.0))
    }
}
