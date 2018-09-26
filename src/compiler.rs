use crate::ident::Ident;
use crate::Function;
use crate::Invoke;
use crate::Print;
use crate::Receiver;
use crate::Type;
use crate::TypeNode;
use crate::ValueNode;
use crate::ValueRef;
use proc_macro2::TokenStream;
use quote::quote;
use ref_cast::RefCast;
use std::collections::BTreeSet as Set;

#[derive(Debug)]
pub(crate) struct Program {
    pub crates: Vec<Ident>,
    pub impls: Vec<CompleteImpl>,
}

#[derive(Debug)]
pub(crate) struct CompleteImpl {
    pub of: Type,
    pub ty: Type,
    pub functions: Vec<CompleteFunction>,
}

#[derive(Debug)]
pub(crate) struct CompleteFunction {
    pub self_ty: Type,
    pub f: Function,
    pub values: Vec<ValueNode>,
    pub invokes: Vec<Invoke>,
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
        let of = Print::ref_cast(&self.of);
        let ty = Print::ref_cast(&self.ty);

        let functions = self.functions.iter().map(CompleteFunction::compile);

        quote! {
            impl #of for #ty {
                #(#functions)*
            }
        }
    }
}

impl CompleteFunction {
    fn compile(&self) -> TokenStream {
        let name = Ident::new(&self.f.name);

        let mut inputs = Vec::new();
        inputs.extend(receiver_tokens(self.f.sig.receiver));
        for (i, input) in self.f.sig.inputs.iter().enumerate() {
            let binding = Ident::new(format!("__arg{}", i));
            let ty = Print::ref_cast(input);
            inputs.push(quote! {
                #binding : #ty
            });
        }

        let output = match self.f.sig.output {
            Type(TypeNode::Unit) => None,
            ref other => Some(Print::ref_cast(other)),
        };

        let reachable = self.compute_reachability();
        let mutable = self.compute_mutability();
        let values = self.refs().flat_map(|v| {
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
            fn #name (#(#inputs),*) #(-> #output)* {
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
            reachable.insert(ret);
        }

        use crate::ValueNode::*;
        while let Some(v) = stack.pop() {
            match self.values[v.0] {
                Unit => {}
                Str(ref s) => {}
                Reference(v) | ReferenceMut(v) | Dereference(v) => {
                    if reachable.insert(v) {
                        stack.push(v);
                    }
                }
                Binding { ref name, .. } => {}
                Invoke(invoke) => {
                    for &v in &self.invokes[invoke.0].args {
                        if reachable.insert(v) {
                            stack.push(v);
                        }
                    }
                }
                Destructure { parent, ref field } => {
                    if reachable.insert(parent) {
                        stack.push(parent);
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
        if let ValueNode::Invoke(_) = self.values[v.0] {
            return true;
        }
        false
    }

    fn compile_value(&self, v: ValueRef) -> TokenStream {
        match self.values[v.0] {
            ValueNode::Unit => quote! { () },
            ValueNode::Str(ref s) => quote! { #s },
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
            ValueNode::Binding { ref name, .. } => quote! { #name },
            ValueNode::Invoke(invoke) => {
                let invoke = &self.invokes[invoke.0];
                let parent = match invoke.function.parent {
                    Some(ref parent) => {
                        let print = Print::ref_cast(parent);
                        Some(quote!(#print ::))
                    }
                    None => None,
                };
                let name = Ident::new(&invoke.function.name);
                let args = invoke.args.iter().map(|value| value.binding());

                quote! {
                    #parent #name ( #(#args),* )
                }
            }
            ValueNode::Destructure { parent, ref field } => {
                let parent = parent.binding();
                quote! {
                    &#parent.#field
                }
            }
            ValueNode::DataStructure { .. } => unimplemented!(),
        }
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
