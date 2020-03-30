use crate::{Ident, Invoke, Lifetime, MacroInvoke, Push, TypeParam, TypedIndex, ValueNode};
use std::cell::RefCell;
use std::thread::LocalKey;

thread_local! {
    pub(crate) static VALUES: RefCell<Vec<ValueNode>> = RefCell::new(Vec::new());
    pub(crate) static INVOKES: RefCell<Vec<Invoke>> = RefCell::new(Vec::new());
    pub(crate) static MACROS: RefCell<Vec<MacroInvoke>> = RefCell::new(Vec::new());
    pub(crate) static TYPE_PARAMS: RefCell<Vec<TypeParam>> = RefCell::new(Vec::new());
    pub(crate) static LIFETIMES: RefCell<Vec<Lifetime>> = {
        let mut vec = Vec::new();
        vec.push(Lifetime {ident: Ident::new("static") });
        RefCell::new(vec)
    };
}

pub(crate) trait GlobalBorrow<T> {
    fn with_borrow<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<T>) -> R;

    fn with_borrow_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<T>) -> R;
}

impl<T> GlobalBorrow<T> for LocalKey<RefCell<Vec<T>>> {
    fn with_borrow<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&Vec<T>) -> R,
    {
        self.with(|data| f(&*data.borrow()))
    }

    fn with_borrow_mut<R, F>(&'static self, f: F) -> R
    where
        F: FnOnce(&mut Vec<T>) -> R,
    {
        self.with(|data| f(&mut *data.borrow_mut()))
    }
}

pub(crate) trait GlobalPush {
    type Element: TypedIndex;

    fn index_push(&'static self, element: Self::Element) -> <Self::Element as TypedIndex>::Index;
}

impl<T> GlobalPush for LocalKey<RefCell<Vec<T>>>
where
    T: TypedIndex,
{
    type Element = T;

    fn index_push(&'static self, element: T) -> T::Index {
        self.with(|data| data.borrow_mut().index_push(element))
    }
}
