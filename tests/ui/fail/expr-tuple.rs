#![allow(unused)]

#[expandable::expr]
macro_rules! a {
    () => {
        (,)
    };
}

#[expandable::expr]
macro_rules! b {
    () => {
        (42,, 42)
    };
}

#[expandable::expr]
macro_rules! c {
    () => {
        (a b)
    }
}

fn main() {}
