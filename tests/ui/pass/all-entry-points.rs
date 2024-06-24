#![allow(unused)]

#[expandable::expr]
macro_rules! e {
    () => {
        42
    };
}

#[expandable::item]
macro_rules! f {
    () => {
        fn f() {}
    };
}

#[expandable::pat]
macro_rules! f {
    () => {
        _
    };
}

#[expandable::stmt]
macro_rules! f {
    () => {
        let _ = 42;
    };
}

#[expandable::ty]
macro_rules! f {
    () => {
        Foo<Bar>
    };
}

fn main() {}
