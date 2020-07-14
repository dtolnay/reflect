use crate::{
    Function, GlobalBorrow, GlobalPush, Ident, InvokeRef, MacroInvokeRef, Parent, Path,
    RuntimeFunction, Type, TypeNode, Value, ValueNode, ValueRef, INVOKES, MACROS, VALUES,
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
        let mut wip = WipFunction {
            self_ty: Some(self.wip.ty.clone()),
            f: f.SELF(),
            values: WipRange::new(ValueRef(VALUES.with_borrow(Vec::len))),
            invokes: WipRange::new(InvokeRef(INVOKES.with_borrow(Vec::len))),
            macros: WipRange::new(MacroInvokeRef(MACROS.with_borrow(Vec::len))),
            ret: None,
        };
        let ret = run(MakeFunction { wip: &wip }).index;
        wip.values.end = Some(ValueRef(VALUES.with_borrow(Vec::len)));
        wip.invokes.end = Some(InvokeRef(INVOKES.with_borrow(Vec::len)));
        wip.macros.end = Some(MacroInvokeRef(MACROS.with_borrow(Vec::len)));
        wip.ret = if ret.is_unit_type() { None } else { Some(ret) };

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
            SelfByReference { is_mut, lifetime } if index == 0 => wip.self_ty.clone().map(|ty| {
                Type(TypeNode::Reference {
                    is_mut,
                    lifetime: lifetime.0,
                    inner: Box::new(ty.0),
                })
            }),
            NoSelf => None,
            SelfByValue | SelfByReference { .. } => {
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
            index: VALUES.index_push(node),
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
            index: VALUES.index_push(node),
        }
    }

    fn string(&self, s: &str) -> Value {
        let node = ValueNode::Str(s.to_owned());
        Value {
            index: VALUES.index_push(node),
        }
    }
}
impl WipImpl {
    pub(crate) fn has_generics(&self) -> bool {
        if let TypeNode::DataStructure(data) = &self.ty.0 {
            !data.generics.params.is_empty()
                || if let Some(parent) = &self.trait_ty {
                    !parent.generics.params.is_empty()
                } else {
                    false
                }
        } else {
            false
        }
    }
}
