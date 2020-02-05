use crate::{Function, Parent, Path, Type, TypeNode};
use std::rc::Rc;

pub trait RuntimeType {
    #[allow(non_snake_case)]
    fn SELF(self) -> Type;
}

pub trait RuntimeFunction {
    #[allow(non_snake_case)]
    fn SELF(self) -> Rc<Function>;
}

pub trait RuntimeParent {
    #[allow(non_snake_case)]
    fn SELF(self) -> Rc<Parent>;
}

pub trait RuntimeTrait: RuntimeParent {}

pub trait RuntimeImpl: RuntimeParent {}

impl RuntimeType for Type {
    fn SELF(self) -> Type {
        self
    }
}

impl RuntimeType for Path {
    fn SELF(self) -> Type {
        Type(TypeNode::Path(self))
    }
}

pub mod prelude {
    use super::*;

    #[allow(non_camel_case_types)]
    #[derive(Copy, Clone)]
    pub struct str;

    impl RuntimeType for str {
        fn SELF(self) -> Type {
            Type::primitive_str()
        }
    }
}
