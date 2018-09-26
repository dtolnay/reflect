use crate::Function;
use crate::Type;

pub trait RuntimeType {
    #[allow(non_snake_case)]
    fn SELF(self) -> Type;
}

pub trait RuntimeFunction {
    #[allow(non_snake_case)]
    fn SELF(self) -> Function;
}

impl RuntimeType for Type {
    fn SELF(self) -> Type {
        self
    }
}

impl RuntimeFunction for Function {
    fn SELF(self) -> Function {
        self
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
