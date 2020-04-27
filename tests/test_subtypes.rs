use reflect::library;

library! {
    use subtypes {
        type Subtypes;

        impl Subtypes {
            fn sub1<'a, 'b, T, U>(&'a T, &'b U) -> &'b U where 'a: 'b;
        }
    }
}
