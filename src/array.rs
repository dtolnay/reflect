pub trait Array<T>: private::Sealed {
    #[doc(hidden)]
    fn first(&self) -> T;
    #[doc(hidden)]
    fn to_vec(&self) -> Vec<T>;
}

macro_rules! impl_array {
    ($($n:tt)*) => {
        $(
            impl<T: Clone> Array<T> for [T; $n] {
                fn first(&self) -> T {
                    self[0].clone()
                }
                fn to_vec(&self) -> Vec<T> {
                    <[_]>::to_vec(self)
                }
            }

            impl<T: Clone> private::Sealed for [T; $n] {}
        )*
    }
}

impl_array!(1 2 3 4 5 6 7 8 9 10);

mod private {
    pub trait Sealed {}
}
