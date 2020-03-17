use crate::{Generics, Invoke, Parent, Push, Signature, Value, ValueNode, WIP};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) parent: Option<Rc<Parent>>,
    pub(crate) name: String,
    pub(crate) sig: Signature,
}

impl Function {
    pub fn invoke(self: Rc<Function>, args: &[Value]) -> Value {
        let wip = WIP.with(Rc::clone);
        let wip = &mut *wip.borrow_mut();
        let wip = wip.as_mut().unwrap();

        let invoke = wip.invokes.index_push(Invoke {
            function: self.clone_with_fresh_generics(),
            args: args.iter().map(|value| value.index).collect(),
        });
        let node = ValueNode::Invoke(invoke);
        Value {
            index: wip.values.index_push(node),
        }
    }

    pub fn get_function(name: &str, sig: Signature) -> Function {
        Function {
            parent: None,
            name: name.to_owned(),
            sig,
        }
    }

    /// When calling set_parent it is important to use a reference to the same
    /// Parent struct for all functions declared inside of the same impl or
    /// trait definition. Otherwise the trait inference may not work correctly
    pub fn set_parent(&mut self, parent: Rc<Parent>) {
        self.parent = Some(parent);
    }

    pub fn clone_with_fresh_generics(self: Rc<Self>) -> Rc<Self> {
        if let Some((parent_generics, mut ref_map)) = self.parent.as_ref().and_then(|parent| {
            parent
                .generics
                .as_ref()
                .map(|generics| generics.clone_with_fresh_generics())
        }) {
            let sig_generics = self.sig.generics.as_ref().map(|generics| Generics {
                params: generics
                    .params
                    .iter()
                    .map(|param| {
                        let new_param = param.get_fresh_param();
                        ref_map.insert(*param, new_param);
                        new_param
                    })
                    .collect(),
                constraints: generics
                    .constraints
                    .iter()
                    .map(|constraint| constraint.clone_with_fresh_generics(&ref_map))
                    .collect(),
            });
            let old_parent = self.parent.as_ref().unwrap();
            let old_sig = &self.sig;

            Rc::new(Function {
                parent: Some(Rc::new(Parent {
                    ty: old_parent.ty.clone_with_fresh_generics(&ref_map),
                    generics: Some(parent_generics),
                    parent_kind: old_parent.parent_kind,
                })),
                name: self.name.clone(),
                sig: Signature {
                    generics: sig_generics,
                    receiver: old_sig.receiver,
                    inputs: old_sig
                        .inputs
                        .iter()
                        .map(|ty| ty.clone_with_fresh_generics(&ref_map))
                        .collect(),
                    output: old_sig.output.clone_with_fresh_generics(&ref_map),
                },
            })
        } else if let Some((sig_generics, ref_map)) = self
            .sig
            .generics
            .as_ref()
            .map(|generics| generics.clone_with_fresh_generics())
        {
            let old_sig = &self.sig;

            Rc::new(Function {
                parent: self.parent.clone(),
                name: self.name.clone(),
                sig: Signature {
                    generics: Some(sig_generics),
                    receiver: old_sig.receiver,
                    inputs: old_sig
                        .inputs
                        .iter()
                        .map(|ty| ty.clone_with_fresh_generics(&ref_map))
                        .collect(),
                    output: old_sig.output.clone_with_fresh_generics(&ref_map),
                },
            })
        } else {
            self.clone()
        }
    }
}
