#![allow(unused)]

// Adapted from:
//
// https://github.com/rust-lang/reference/tree/51817951d0d213a0011f82b62aae02c3b3f2472e
#[expandable::expr]
macro_rules! test {
    () => {{
        x;
        x::y::z;

        // TOOO: simple paths
        // #[rustfmt::skip]
        // struct Foo;

        (0..10).collect::<Vec<_>>();
        Vec::<u8>::with_capacity(1024);

        S::f();
        <S as T1>::f();
        <S as T2>::f();

        // TODO: impl trait
        // impl ops::Index<ops::Range<usize>> for S { /*...*/ }
        // TODO: lifetimes
        // fn i<'a>() -> impl Iterator<Item = ops::Example<'a>> {
        // ...
        // }
        // TODO: type item
        // type G = std::boxed::Box<dyn std::ops::FnOnce(isize) -> isize>;

        ::std::time::Instant::now();

        ::a::foo();

        self::foo();

        // TODO: trait definition
        // trait T {
        //     type Item;
        //     const C: i32;
        //     // `Self` will be whatever type that implements `T`.
        //     fn new() -> Self;
        //     // `Self::Item` will be the type alias in the implementation.
        //     fn f(&self) -> Self::Item;
        // }
        // TODO: struct definition
        // struct S;
        // impl T for S {
        //     type Item = i32;
        //     const C: i32 = 9;
        //     fn new() -> Self {
        //         // `Self` is the type `S`.
        //         S
        //     }
        //     fn f(&self) -> Self::Item {
        //         // `Self::Item` is the type `i32`.
        //         Self::C // `Self::C` is the constant value `9`.
        //     }
        // }

        super::a::foo();

        super::super::foo();
        self::super::super::foo();

        crate::foo();
    }};
}

fn main() {}
