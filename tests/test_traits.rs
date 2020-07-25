use quote::quote;
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

            fn single_impl(&impl Trait);
            fn double_impl(&(impl Trait + AutoTrait));
            fn generic_impl<T>(&(impl Generic<T> + 'static));
        }

         trait SimpleTrait {
            fn simple(&self);
        }
    }
}

fn derive1(ex: Execution) {
    ex.make_trait_impl(RUNTIME::Mod::SimpleTrait, ex.target_type(), |block| {
        block.make_function(RUNTIME::Mod::SimpleTrait::simple, dyn_traits);
    });
}

fn dyn_traits(f: MakeFunction) -> Value {
    let receiver = f.arg(0);

    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(_receiver) => unimplemented!(),
            Struct::Tuple(_receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                for field in receiver.fields() {
                    RUNTIME::Mod::Struct::single_dyn.INVOKE(field.get_value());
                    RUNTIME::Mod::Struct::double_dyn.INVOKE(field.get_value());
                }
                f.unit()
            }
        },
        _ => unimplemented!(),
    }
}

#[test]
fn test_dyn_traits() {
    let input = quote! {
        struct Generic<T, U> {
            name: String,
            one: T,
            two: U,
        }
    };

    // TODO: remove trait inference redundancy
    let expected = quote! {
        impl<__T0, __T1> ::Mod::SimpleTrait for Generic<__T0, __T1>
        where
            __T0: ::Mod::Trait + ::Mod::AutoTrait,
            __T1: ::Mod::Trait + ::Mod::AutoTrait,
            __T1: ::Mod::Trait,
            __T0: ::Mod::Trait,
        {
            fn simple<'__a1>(&'__a1 self) {
                let __v0 = self;
                let __v1 = &__v0.name;
                let __v2 = &__v0.one;
                let __v3 = &__v0.two;
                let _ = ::Mod::Struct::single_dyn(__v1);
                let _ = ::Mod::Struct::double_dyn(__v1);
                let _ = ::Mod::Struct::single_dyn(__v2);
                let _ = ::Mod::Struct::double_dyn(__v2);
                let _ = ::Mod::Struct::single_dyn(__v3);
                let _ = ::Mod::Struct::double_dyn(__v3);
            }
        }
    };

    let actual = reflect::derive(input, derive1);
    assert_eq!(actual.to_string(), expected.to_string());
}

fn derive2(ex: Execution) {
    ex.make_trait_impl(RUNTIME::Mod::SimpleTrait, ex.target_type(), |block| {
        block.make_function(RUNTIME::Mod::SimpleTrait::simple, impl_traits);
    });
}

fn impl_traits(f: MakeFunction) -> Value {
    let receiver = f.arg(0);

    match receiver.data() {
        Data::Struct(receiver) => match receiver {
            Struct::Unit(_receiver) => unimplemented!(),
            Struct::Tuple(_receiver) => unimplemented!(),
            Struct::Struct(receiver) => {
                for field in receiver.fields() {
                    RUNTIME::Mod::Struct::single_impl.INVOKE(field.get_value());
                    RUNTIME::Mod::Struct::double_impl.INVOKE(field.get_value());
                }
                f.unit()
            }
        },
        _ => unimplemented!(),
    }
}

#[test]
fn test_impl_traits() {
    let input = quote! {
        struct Generic<T, U> {
            name: String,
            one: T,
            two: U,
        }
    };

    // TODO: remove trait inference redundancy
    let expected = quote! {
        impl<__T0, __T1> ::Mod::SimpleTrait for Generic<__T0, __T1>
        where
            __T0: ::Mod::Trait + ::Mod::AutoTrait,
            __T1: ::Mod::Trait + ::Mod::AutoTrait,
            __T1: ::Mod::Trait,
            __T0: ::Mod::Trait,
        {
            fn simple<'__a1>(&'__a1 self) {
                let __v0 = self;
                let __v1 = &__v0.name;
                let __v2 = &__v0.one;
                let __v3 = &__v0.two;
                let _ = ::Mod::Struct::single_impl(__v1);
                let _ = ::Mod::Struct::double_impl(__v1);
                let _ = ::Mod::Struct::single_impl(__v2);
                let _ = ::Mod::Struct::double_impl(__v2);
                let _ = ::Mod::Struct::single_impl(__v3);
                let _ = ::Mod::Struct::double_impl(__v3);
            }
        }
    };

    let actual = reflect::derive(input, derive2);
    assert_eq!(actual.to_string(), expected.to_string());
}
