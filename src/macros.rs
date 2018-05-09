#[macro_export]
macro_rules! library {
    ($(extern crate $n:ident { $($tt:tt)* })*) => {
        #[allow(non_snake_case)]
        mod RUNTIME {
            #[allow(dead_code, non_snake_case)]
            pub fn MODULE() -> $crate::Module {
                $crate::Module::root()
            }

            $(
                library!(@mod $n { $($tt)* });
            )*
        }
    };

    (@mod $n:ident { $($tt:tt)* }) => {
        pub mod $n {
            #[allow(unused_imports)]
            use $crate::runtime::prelude::*;

            #[allow(dead_code, non_snake_case)]
            pub fn MODULE() -> $crate::Module {
                super::MODULE().get_module(stringify!($n))
            }

            struct __Indirect<T>(T);

            library!(@parse $($tt)*);
        }
    };

    (@type $n:ident) => {
        #[derive(Copy, Clone)]
        pub struct $n;

        impl $crate::runtime::RuntimeType for $n {
            fn SELF(self) -> $crate::Type {
                MODULE().get_type(stringify!($n))
            }
        }
    };

    (@impl $n:ident { fn $f:ident($($args:tt)*) $($rest:tt)* }) => {
        library!(@fn_args $n { fn () $f($($args)*) $($rest)*});
    };

    (@impl $n:ident {}) => {};

    (@fn_args $n:ident { fn ($(($($arg:tt)*))*) $f:ident() -> $($rest:tt)* }) => {
        library!(@parse_type (@fn_ret $n { fn ($(($($arg)*))*) $f }) $($rest)*);
    };

    (@fn_args $n:ident { fn ($(($($arg:tt)*))*) $f:ident() ; $($rest:tt)* }) => {
        library!(@fn_args $n { fn ($(($($arg)*))*) $f() -> (); $($rest)* });
    };

    (@fn_args $n:ident { fn ($(($($arg:tt)*))*) $f:ident($($args:tt)*) $($rest:tt)* }) => {
        library!(@parse_type (@fn_arg $n { fn ($(($($arg)*))*) $f $($rest)* }) $($args)*);
    };

    (@fn_arg ($($ty:tt)*) (, $($more:tt)*) $n:ident { fn ($(($($arg:tt)*))*) $f:ident $($rest:tt)* }) => {
        library!(@fn_args $n { fn ($(($($arg)*))* (v $($ty)*)) $f($($more)*) $($rest)* });
    };

    (@fn_arg ($($ty:tt)*) () $n:ident { fn ($(($($arg:tt)*))*) $f:ident $($rest:tt)* }) => {
        library!(@fn_args $n { fn ($(($($arg)*))* (v $($ty)*)) $f() $($rest)* });
    };

    (@fn_ret ($($ty:tt)*) (; $($rest:tt)*) $n:ident { fn ($(($($arg:tt)*))*) $f:ident }) => {
        library!(@fn_recv $n $f ($(($($arg)*))*) ($($ty)*));
        library!(@impl $n { $($rest)* });
    };

    (@fn_recv $n:ident $f:ident ((v self) $(($($arg:tt)*))*) ($($ret:tt)*)) => {
        library!(@fn $n $f (v set_self_by_value) ($(($($arg)*))*) ($($ret)*));
    };

    (@fn_recv $n:ident $f:ident ((v &self) $(($($arg:tt)*))*) ($($ret:tt)*)) => {
        library!(@fn $n $f (v set_self_by_reference) ($(($($arg)*))*) ($($ret)*));
    };

    (@fn_recv $n:ident $f:ident ((v &mut self) $(($($arg:tt)*))*) ($($ret:tt)*)) => {
        library!(@fn $n $f (v set_self_by_reference_mut) ($(($($arg)*))*) ($($ret)*));
    };

    (@fn_recv $n:ident $f:ident ($(($($arg:tt)*))*) ($($ret:tt)*)) => {
        library!(@fn $n $f () ($(($($arg)*))*) ($($ret)*));
    };

    (@fn $n:ident $f:ident ($($v:ident $recv:ident)*) ($(($w:ident $($arg:tt)*))*) ($($ret:tt)*)) => {
        impl __Indirect<$n> {
            #[allow(dead_code)]
            fn $f() {
                #[allow(non_camel_case_types)]
                #[derive(Copy, Clone)]
                pub struct $f;

                impl $crate::runtime::RuntimeFunction for $f {
                    fn SELF(self) -> $crate::Function {
                        let mut sig = $crate::Signature::new();
                        $(
                            sig.$recv();
                        )*
                        $(
                            sig.add_input(library!(@to_runtime_type $($arg)*));
                        )*
                        sig.set_output(library!(@to_runtime_type $($ret)*));
                        $crate::runtime::RuntimeType::SELF($n).get_function(stringify!($f), sig)
                    }
                }

                impl $f {
                    pub fn INVOKE<'a>(
                        self,
                        $(
                            $v: $crate::Value<'a>,
                        )*
                        $(
                            $w: $crate::Value<'a>,
                        )*
                    ) -> $crate::Value<'a> {
                        $crate::runtime::RuntimeFunction::SELF(self).invoke([$($v,)* $($w,)*])
                    }
                }

                impl $n {
                    #[allow(non_upper_case_globals)]
                    pub const $f: $f = $f;
                }
            }
        }
    };

    (@to_runtime_type $n:ident) => {
        $crate::runtime::RuntimeType::SELF($n)
    };

    (@to_runtime_type &mut $($rest:tt)*) => {
        library!(@to_runtime_type $($rest)*).reference_mut()
    };

    (@to_runtime_type & $($rest:tt)*) => {
        library!(@to_runtime_type $($rest)*).reference()
    };

    (@to_runtime_type ()) => {
        $crate::Type::unit()
    };

    (@to_runtime_type $($rest:tt)*) => {
        compile_error!(stringify!($($rest)*))
    };

    (@parse_type (@$tag:ident $($then:tt)*) () $($rest:tt)*) => {
        library!(@$tag (()) ($($rest)*) $($then)*);
    };

    (@parse_type (@$tag:ident $($then:tt)*) $t:ident $($rest:tt)*) => {
        library!(@$tag ($t) ($($rest)*) $($then)*);
    };

    (@parse_type (@$tag:ident $($then:tt)*) &mut $($rest:tt)*) => {
        library!(@parse_type (@parse_type_mut @$tag $($then)*) $($rest)*);
    };

    (@parse_type_mut ($($ty:tt)*) ($($rest:tt)*) @$tag:ident $($then:tt)*) => {
        library!(@$tag (&mut $($ty)*) ($($rest)*) $($then)*);
    };

    (@parse_type (@$tag:ident $($then:tt)*) & $($rest:tt)*) => {
        library!(@parse_type (@parse_type_ref @$tag $($then)*) $($rest)*);
    };

    (@parse_type_ref ($($ty:tt)*) ($($rest:tt)*) @$tag:ident $($then:tt)*) => {
        library!(@$tag (& $($ty)*) ($($rest)*) $($then)*);
    };

    (@parse_type (@$tag:ident $($then:tt)*) $other:tt $($input:tt)*) => {
        error_token!($other);
    };

    (@parse mod $n:ident { $($tt:tt)* } $($rest:tt)*) => {
        library!(@mod $n { $($tt)* });
        library!(@parse $($rest)*);
    };

    (@parse type $n:ident; $($rest:tt)*) => {
        library!(@type $n);
        library!(@parse $($rest)*);
    };

    (@parse impl $n:ident { $($tt:tt)* } $($rest:tt)*) => {
        library!(@impl $n { $($tt)* });
        library!(@parse $($rest)*);
    };

    (@parse) => {};
}

#[doc(hidden)]
#[macro_export]
macro_rules! error_token {
    () => {};
}
