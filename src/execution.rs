use crate::{Ident, MakeImpl, Module, Parent, Path, RuntimeTrait, RuntimeType, Type, WipImpl};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Copy)]
pub struct Execution<'a> {
    pub(crate) ty: &'a Type,
    pub(crate) tracker: &'a Tracker,
}

#[derive(Debug, Clone)]
pub(crate) struct Tracker {
    pub(crate) crates: RefCell<Vec<Ident>>,
    pub(crate) impls: RefCell<Vec<WipImpl>>,
}

impl<'a> Execution<'a> {
    pub fn load_crate(self, name: &str) -> Module {
        self.tracker.load_crate(name)
    }

    pub fn make_trait_impl<TraitType, SelfType>(
        self,
        trait_type: TraitType,
        self_type: SelfType,
        run: fn(MakeImpl),
    ) where
        TraitType: RuntimeTrait,
        SelfType: RuntimeType,
    {
        self.tracker
            .make_trait_impl(trait_type.SELF(), self_type.SELF(), run);
    }

    pub fn target_type(self) -> Type {
        self.ty.clone()
    }
}

impl Tracker {
    pub(crate) fn new() -> Self {
        Tracker {
            crates: RefCell::new(Vec::new()),
            impls: RefCell::new(Vec::new()),
        }
    }

    fn load_crate(&self, name: &str) -> Module {
        self.crates.borrow_mut().push(Ident::new(name));
        Module {
            path: Path::empty().get_simple_path(name),
        }
    }

    fn make_trait_impl(&self, trait_ty: Rc<Parent>, ty: Type, run: fn(MakeImpl)) {
        let wip = WipImpl {
            trait_ty: Some(trait_ty),
            ty,
            functions: RefCell::new(Vec::new()),
        };
        run(MakeImpl { wip: &wip });
        self.impls.borrow_mut().push(wip);
    }
}
