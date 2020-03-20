use crate::{Ident, Invoke, Lifetime, MacroInvoke, TypeParam, ValueNode, ValueRef};
use std::cell::RefCell;
use std::default::Default;
use std::thread::LocalKey;

thread_local!(pub(crate) static GLOBAL_DATA: RefCell<GlobalData> = RefCell::new(Default::default()));

pub(crate) struct GlobalData {
    pub(crate) values: Vec<ValueNode>,
    pub(crate) invokes: Vec<Invoke>,
    pub(crate) macros: Vec<MacroInvoke>,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) lifetimes: Vec<Lifetime>,
}

impl Default for GlobalData {
    fn default() -> Self {
        Self {
            values: Vec::new(),
            invokes: Vec::new(),
            macros: Vec::new(),
            type_params: Vec::new(),
            lifetimes: vec![Lifetime {
                ident: Ident::new("static"),
            }],
        }
    }
}

pub(crate) trait GlobalBorrow {
    fn with_borrow<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&GlobalData) -> R;

    fn with_borrow_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut GlobalData) -> R;

    fn with_borrow_values<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<ValueNode>) -> R;

    fn with_borrow_values_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<ValueNode>) -> R;

    fn with_borrow_invokes<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<Invoke>) -> R;

    fn with_borrow_invokes_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<Invoke>) -> R;

    fn with_borrow_macros<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<MacroInvoke>) -> R;

    fn with_borrow_macros_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<MacroInvoke>) -> R;

    fn with_borrow_type_params<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<TypeParam>) -> R;

    fn with_borrow_type_params_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<TypeParam>) -> R;

    fn with_borrow_lifetimes<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<Lifetime>) -> R;

    fn with_borrow_lifetimes_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<Lifetime>) -> R;
}

impl GlobalBorrow for LocalKey<RefCell<GlobalData>> {
    fn with_borrow<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&GlobalData) -> R,
    {
        self.with(|data| f(&*data.borrow()))
    }

    fn with_borrow_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut GlobalData) -> R,
    {
        self.with(|data| f(&mut *data.borrow_mut()))
    }

    fn with_borrow_values<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<ValueNode>) -> R,
    {
        self.with(|data| f(&data.borrow().values))
    }

    fn with_borrow_values_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<ValueNode>) -> R,
    {
        self.with(|data| f(&mut data.borrow_mut().values))
    }

    fn with_borrow_invokes<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<Invoke>) -> R,
    {
        self.with(|data| f(&data.borrow().invokes))
    }

    fn with_borrow_invokes_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<Invoke>) -> R,
    {
        self.with(|data| f(&mut data.borrow_mut().invokes))
    }

    fn with_borrow_macros<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<MacroInvoke>) -> R,
    {
        self.with(|data| f(&data.borrow().macros))
    }

    fn with_borrow_macros_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<MacroInvoke>) -> R,
    {
        self.with(|data| f(&mut data.borrow_mut().macros))
    }

    fn with_borrow_type_params<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<TypeParam>) -> R,
    {
        self.with(|data| f(&data.borrow().type_params))
    }

    fn with_borrow_type_params_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<TypeParam>) -> R,
    {
        self.with(|data| f(&mut data.borrow_mut().type_params))
    }

    fn with_borrow_lifetimes<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<Lifetime>) -> R,
    {
        self.with(|data| f(&data.borrow().lifetimes))
    }

    fn with_borrow_lifetimes_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<Lifetime>) -> R,
    {
        self.with(|data| f(&mut data.borrow_mut().lifetimes))
    }
}

impl GlobalData {
    pub(crate) fn node(&self, index: ValueRef) -> ValueNode {
        GLOBAL_DATA.with_borrow_values(|values| values[index.0].clone())
    }
}
