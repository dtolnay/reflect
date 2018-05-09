use Function;
use Ident;
use Push;
use RuntimeFunction;
use Type;
use Value;
use ValueNode;
use ValueRef;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct MakeImpl {
    pub(crate) wip: Rc<RefCell<WipImpl>>,
}

#[derive(Debug, Clone)]
pub(crate) struct WipImpl {
    pub(crate) of: Type,
    pub(crate) ty: Type,
    pub(crate) functions: Vec<Rc<RefCell<WipFunction>>>,
}

#[derive(Debug, Clone)]
pub struct MakeFunction<'a> {
    pub(crate) wip: &'a WipFunction,
}

#[derive(Debug, Clone)]
pub(crate) struct WipFunction {
    pub(crate) self_ty: Type,
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
        let mut wip = WipFunction {
            self_ty: self.wip.borrow().ty.clone(),
            f: f.SELF(),
            values: RefCell::new(Vec::new()),
            invokes: RefCell::new(Vec::new()),
            ret: None,
        };
        wip.ret = Some(run(MakeFunction { wip: &wip }).index);
        self.wip
            .borrow_mut()
            .functions
            .push(Rc::new(RefCell::new(wip)));
    }
}

impl<'a> MakeFunction<'a> {
    pub fn unit(&self) -> Value<'a> {
        self.wip.unit()
    }

    pub fn string(&self, s: &str) -> Value<'a> {
        self.wip.string(s)
    }

    pub fn arg(&self, mut index: usize) -> Value<'a> {
        use Receiver::*;

        let node = match match self.wip.f.sig.receiver {
            SelfByValue if index == 0 => Some(self.wip.self_ty.clone()),
            SelfByReference if index == 0 => Some(self.wip.self_ty.clone().reference()),
            SelfByReferenceMut if index == 0 => Some(self.wip.self_ty.clone().reference_mut()),
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
                ty: self.wip.f.sig.inputs[index].clone(),
            },
        };

        Value {
            function: self.wip,
            index: self.wip.values.borrow_mut().index_push(node),
        }
    }
}

impl WipFunction {
    pub(crate) fn node(&self, index: ValueRef) -> ValueNode {
        self.values.borrow()[index.0].clone()
    }

    fn unit(&self) -> Value {
        let node = ValueNode::Unit;
        Value {
            function: self,
            index: self.values.borrow_mut().index_push(node),
        }
    }

    fn string(&self, s: &str) -> Value {
        let node = ValueNode::Str(s.to_owned());
        Value {
            function: self,
            index: self.values.borrow_mut().index_push(node),
        }
    }
}
