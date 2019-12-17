use reflect::*;

library! {
    use Mod {
        type Struct;
        trait Trait {}
        trait AutoTrait {}
        trait Generic<T> {}

        impl Struct {
            fn single_dyn(&dyn Trait);
            fn double_dyn(&(dyn Trait + AutoTrait));
            fn generic_dyn<T>(&(dyn Generic<T> + 'static));
        }
    }
}
