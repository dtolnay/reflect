use crate::Function;
use crate::Ident;
use crate::Push;
use crate::RuntimeFunction;
use crate::Type;
use crate::Value;
use crate::ValueNode;
use crate::ValueRef;
use crate::WIP;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct MakeImpl {
    pub(crate) wip: Rc<RefCell<WipImpl>>,
}

#[derive(Debug, Clone)]
pub(crate) struct WipImpl {
    pub(crate) trait_ty: Option<Type>,
    pub(crate) ty: Type,
    pub(crate) functions: Vec<Rc<RefCell<WipFunction>>>,
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
    pub(crate) values: RefCell<Vec<ValueNode>>,
    pub(crate) invokes: RefCell<Vec<Invoke>>,
    pub(crate) ret: Option<ValueRef>,
}

#[derive(Debug, Clone)]
pub(crate) struct Invoke {
    pub(crate) function: Function,
    pub(crate) args: Vec<ValueRef>,
}

impl MakeImpl {
    pub fn make_function<F>(&self, f: F, run: fn(MakeFunction) -> Value)
    where
        F: RuntimeFunction,
    {
        WIP.with(|old_wip| {
            *old_wip.borrow_mut() = Some(WipFunction {
                self_ty: Some(self.wip.borrow().ty.clone()),
                f: f.SELF(),
                values: RefCell::new(Vec::new()),
                invokes: RefCell::new(Vec::new()),
                ret: None,
            })
        });

        let ret = Some(run(MakeFunction { private: () }).index);
        let mut wip = WIP.with(|wip| wip.borrow_mut().take().unwrap());
        wip.ret = ret;

        self.wip
            .borrow_mut()
            .functions
            .push(Rc::new(RefCell::new(wip)));
    }
}

impl MakeFunction {
    pub fn unit(&self) -> Value {
        WIP.with(|wip| wip.borrow().as_ref().unwrap().unit())
    }

    pub fn string(&self, s: &str) -> Value {
        WIP.with(|wip| wip.borrow().as_ref().unwrap().string(s))
    }

    pub fn arg(&self, mut index: usize) -> Value {
        use crate::Receiver::*;
        let wip = WIP.with(Rc::clone);
        let wip = wip.borrow();
        let wip = wip.as_ref().unwrap();

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
            index: wip.values.borrow_mut().index_push(node),
        };
        value
    }
}

impl WipFunction {
    pub(crate) fn node(&self, index: ValueRef) -> ValueNode {
        self.values.borrow()[index.0].clone()
    }

    fn unit(&self) -> Value {
        let node = ValueNode::Unit;
        Value {
            index: self.values.borrow_mut().index_push(node),
        }
    }

    fn string(&self, s: &str) -> Value {
        let node = ValueNode::Str(s.to_owned());
        Value {
            index: self.values.borrow_mut().index_push(node),
        }
    }
}
