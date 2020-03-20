use crate::{
    Function, GlobalBorrow, Ident, InvokeRef, MacroInvokeRef, Parent, Path, RuntimeFunction, Type,
    Value, ValueNode, ValueRef, GLOBAL_DATA,
};
use std::cell::RefCell;
use std::ops::Range;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct MakeImpl<'a> {
    pub(crate) wip: &'a WipImpl,
}

#[derive(Debug, Clone)]
pub(crate) struct WipImpl {
    pub(crate) trait_ty: Option<Rc<Parent>>,
    pub(crate) ty: Type,
    pub(crate) functions: RefCell<Vec<WipFunction>>,
}

#[derive(Debug, Clone)]
pub struct MakeFunction<'a> {
    wip: &'a WipFunction,
}

#[derive(Debug, Clone)]
pub(crate) struct WipFunction {
    // self_ty is None for freestanding functions
    pub(crate) self_ty: Option<Type>,
    pub(crate) f: Rc<Function>,
    pub(crate) values: WipRange<ValueRef>,
    pub(crate) invokes: WipRange<InvokeRef>,
    pub(crate) macros: WipRange<MacroInvokeRef>,
    pub(crate) ret: Option<ValueRef>,
}

#[derive(Debug, Clone)]
pub(crate) struct Invoke {
    pub(crate) function: Rc<Function>,
    pub(crate) args: Vec<ValueRef>,
}

#[derive(Debug, Clone)]
pub(crate) struct MacroInvoke {
    pub(crate) macro_path: Path,
    pub(crate) args: Vec<ValueRef>,
}

/// A range where the end may not yet have been determined
#[derive(Debug, Clone)]
pub(crate) struct WipRange<Idx> {
    pub(crate) start: Idx,
    pub(crate) end: Option<Idx>,
}

impl<Idx> From<WipRange<Idx>> for Option<Range<Idx>> {
    fn from(wip_range: WipRange<Idx>) -> Self {
        match wip_range.end {
            Some(end) => Some(Range {
                start: wip_range.start,
                end,
            }),
            None => None,
        }
    }
}

impl<Idx> WipRange<Idx> {
    pub(crate) fn new(start: Idx) -> Self {
        Self { start, end: None }
    }
}

impl<'a> MakeImpl<'a> {
    pub fn make_function<F>(&self, f: F, run: fn(MakeFunction) -> Value)
    where
        F: RuntimeFunction,
    {
        let f = f.SELF();
        let mut wip = GLOBAL_DATA.with_borrow(|global| WipFunction {
            self_ty: Some(self.wip.ty.clone()),
            f,
            values: WipRange::new(ValueRef(global.values.len())),
            invokes: WipRange::new(InvokeRef(global.invokes.len())),
            macros: WipRange::new(MacroInvokeRef(global.macros.len())),
            ret: None,
        });
        let ret = Some(run(MakeFunction { wip: &wip }).index);
        GLOBAL_DATA.with_borrow(|global| {
            wip.values.end = Some(ValueRef(global.values.len()));
            wip.invokes.end = Some(InvokeRef(global.invokes.len()));
            wip.macros.end = Some(MacroInvokeRef(global.macros.len()));
        });
        wip.ret = ret;

        self.wip.functions.borrow_mut().push(wip);
    }
}

impl<'a> MakeFunction<'a> {
    pub fn unit(&self) -> Value {
        self.wip.unit()
    }

    pub fn string(&self, s: &str) -> Value {
        self.wip.string(s)
    }

    pub fn arg(&self, mut index: usize) -> Value {
        use crate::Receiver::*;
        let wip = self.wip;

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
        Value {
            index: node.index_push(),
        }
    }
}

impl WipFunction {
    pub(crate) fn node(&self, index: ValueRef) -> ValueNode {
        index.node()
    }

    fn unit(&self) -> Value {
        let node = ValueNode::Tuple(Vec::new());
        Value {
            index: node.index_push(),
        }
    }

    fn string(&self, s: &str) -> Value {
        let node = ValueNode::Str(s.to_owned());
        Value {
            index: node.index_push(),
        }
    }
}
