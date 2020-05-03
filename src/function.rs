use crate::{Generics, GlobalPush, Invoke, Parent, Signature, Value, ValueNode, INVOKES, VALUES};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<Rc<Parent>>,
    pub(crate) name: String,
    pub(crate) sig: Signature,
}

impl Function {
    pub fn invoke(self: Rc<Function>, args: &[Value]) -> Value {
        let function = self.clone_with_fresh_generics();
        let invoke = INVOKES.index_push(Invoke {
            function,
            args: args.iter().map(|value| value.index).collect(),
        });
        let node = ValueNode::Invoke(invoke);
        Value {
            index: VALUES.index_push(node),
        }
    }

    pub fn get_function(name: &str, mut sig: Signature) -> Function {
        sig.insert_elided_lifetimes();
        Function {
            parent: None,
            name: name.to_owned(),
            sig,
        }
    }

    /// When calling `set_parent` it is important to use a reference to the
    /// same Parent struct for all functions declared inside of the same impl
    /// or trait definition. Otherwise the trait inference may not work
    /// correctly.
    pub fn set_parent(&mut self, parent: Rc<Parent>) {
        self.parent = Some(parent);
    }

    pub fn clone_with_fresh_generics(self: Rc<Self>) -> Rc<Self> {
        if let Some((parent, mut param_map)) = self
            .parent
            .as_ref()
            .map(|parent| parent.clone_with_fresh_generics())
        {
            let generics = &self.sig.generics;
            let sig_generics = Generics {
                params: generics
                    .params
                    .iter()
                    .map(|param| {
                        let new_param = param.get_fresh_param();
                        param_map.insert(*param, new_param);
                        new_param
                    })
                    .collect(),
                constraints: generics
                    .constraints
                    .iter()
                    .map(|constraint| constraint.clone_with_fresh_generics(&param_map))
                    .collect(),
                param_map: generics.param_map.clone_with_fresh_generics(&param_map),
            };
            let old_parent = self.parent.as_ref().unwrap();
            let old_sig = &self.sig;

            Rc::new(Function {
                parent: Some(Rc::new(parent)),
                name: self.name.clone(),
                sig: Signature {
                    generics: sig_generics,
                    receiver: old_sig.receiver.clone_with_fresh_generics(&param_map),
                    inputs: old_sig
                        .inputs
                        .iter()
                        .map(|ty| ty.clone_with_fresh_generics(&param_map))
                        .collect(),
                    output: old_sig.output.clone_with_fresh_generics(&param_map),
                },
            })
        } else if !self.sig.generics.params.is_empty() {
            let (sig_generics, param_map) = self.sig.generics.clone_with_fresh_generics();
            let old_sig = &self.sig;

            Rc::new(Function {
                parent: self.parent.clone(),
                name: self.name.clone(),
                sig: Signature {
                    generics: sig_generics,
                    receiver: old_sig.receiver.clone_with_fresh_generics(&param_map),
                    inputs: old_sig
                        .inputs
                        .iter()
                        .map(|ty| ty.clone_with_fresh_generics(&param_map))
                        .collect(),
                    output: old_sig.output.clone_with_fresh_generics(&param_map),
                },
            })
        } else {
            self.clone()
        }
    }
}
