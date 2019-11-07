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
