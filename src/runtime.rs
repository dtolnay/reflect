use crate::{Function, Path, TraitBound, Type, TypeNode, TypeParamBound};

pub trait RuntimeType {
    #[allow(non_snake_case)]
    fn SELF(self) -> Type;
}

pub trait RuntimeFunction {
    #[allow(non_snake_case)]
    fn SELF(self) -> Function;
}

pub trait RuntimeTrait {
    #[allow(non_snake_case)]
    fn SELF(self) -> Path;
}

pub trait RuntimeTraitObject {
    #[allow(non_snake_case)]
    fn SELF(self) -> Type;
}

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

impl RuntimeFunction for Function {
    fn SELF(self) -> Function {
        self
    }
}

impl RuntimeTrait for Path {
    fn SELF(self) -> Path {
        self
    }
}

// Could be changed to [Path; N] when const generics arrives, to avoid cloning
impl RuntimeTraitObject for &[Path] {
    fn SELF(self) -> Type {
        Type(TypeNode::TraitObject(
            self.iter()
                .cloned()
                .map(|path| {
                    TypeParamBound::Trait(TraitBound {
                        lifetimes: Vec::new(),
                        path,
                    })
                })
                .collect(),
        ))
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
