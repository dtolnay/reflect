#![allow(non_upper_case_globals)] // rustc bug: https://github.com/rust-lang/rust/issues/110573
#![allow(clippy::wildcard_imports)]

use reflect::*;

library! {
    use Mod {
        type Struct;
        trait Trait {}
        trait AutoTrait {}

        impl Struct {
            fn single_dyn(&dyn Trait);
            fn double_dyn(&(dyn Trait + AutoTrait));
        }
    }
}
