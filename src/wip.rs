use crate::{
    Function, Ident, Push, RuntimeFunction, StaticBorrow, Type, Value, ValueNode, ValueRef, WIP,
};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct MakeImpl<'a> {
    pub(crate) wip: &'a WipImpl,
}

#[derive(Debug, Clone)]
pub(crate) struct WipImpl {
    pub(crate) trait_ty: Option<Type>,
    pub(crate) ty: Type,
    pub(crate) functions: RefCell<Vec<WipFunction>>,
}

#[derive(Debug, Clone)]
pub struct MakeFunction {
    private: (),
}

#[derive(Debug, Clone)]
pub(crate) struct WipFunction {
    // self_ty is None for freestanding functions
    pub(crate) self_ty: Option<Type>,
    pub(crate) f: Function,
    pub(crate) values: Vec<ValueNode>,
    pub(crate) invokes: Vec<Invoke>,
    pub(crate) macros: Vec<MacroInvoke>,
    pub(crate) ret: Option<ValueRef>,
}

#[derive(Debug, Clone)]
pub(crate) struct Invoke {
    pub(crate) function: Function,
    pub(crate) args: Vec<ValueRef>,
}

#[derive(Debug, Clone)]
pub(crate) struct MacroInvoke {
    pub(crate) macro_name: String,
    pub(crate) args: Vec<ValueRef>,
}

impl<'a> MakeImpl<'a> {
    pub fn make_function<F>(&self, f: F, run: fn(MakeFunction) -> Value)
    where
        F: RuntimeFunction,
    {
        WIP.with(|old_wip| {
            *old_wip.borrow_mut() = Some(WipFunction {
                self_ty: Some(self.wip.ty.clone()),
                f: f.SELF(),
                values: Vec::new(),
                invokes: Vec::new(),
                macros: Vec::new(),
                ret: None,
            })
        });

        let ret = Some(run(MakeFunction { private: () }).index);
        let mut wip = WIP.with(|wip| wip.borrow_mut().take().unwrap());
        wip.ret = ret;

        self.wip.functions.borrow_mut().push(wip);
    }
}

impl MakeFunction {
    pub fn unit(&self) -> Value {
        WIP.with_borrow_mut(|wip| wip.unit())
    }

    pub fn string(&self, s: &str) -> Value {
        WIP.with_borrow_mut(|wip| wip.string(s))
    }

    pub fn arg(&self, mut index: usize) -> Value {
        use crate::Receiver::*;
        let wip = WIP.with(Rc::clone);
        let wip = &mut *wip.borrow_mut();
        let wip = wip.as_mut().unwrap();

        let node = match match wip.f.sig.receiver {
            SelfByValue if index == 0 => wip.self_ty.clone(),
            SelfByReference if index == 0 => wip.self_ty.clone().map(|ty| ty.reference()),
            SelfByReferenceMut if index == 0 => wip.self_ty.clone().map(|ty| ty.reference_mut()),
            NoSelf => None,
            SelfByValue | SelfByReference | SelfByReferenceMut => {
                index -= 1;
                None
            }
        } {
            Some(receiver) => ValueNode::Binding {
                name: Ident::new("self"),
                ty: receiver,
            },
            None => ValueNode::Binding {
                name: Ident::new(format!("__arg{}", index)),
                ty: wip.f.sig.inputs[index].clone(),
            },
        };
        let value = Value {
            index: wip.values.index_push(node),
        };
        value
    }
}

impl WipFunction {
    pub(crate) fn node(&self, index: ValueRef) -> ValueNode {
        self.values[index.0].clone()
    }

    fn unit(&mut self) -> Value {
        let node = ValueNode::Tuple(Vec::new());
        Value {
            index: self.values.index_push(node),
        }
    }

    fn string(&mut self, s: &str) -> Value {
        let node = ValueNode::Str(s.to_owned());
        Value {
            index: self.values.index_push(node),
        }
    }
}
