#![allow(unused)]

#[expandable::expr]
macro_rules! a {
    () => {
        loop loop {}
    };
}

#[expandable::expr]
macro_rules! b {
    () => {
        while < 42 {}
    };
}

#[expandable::expr]
macro_rules! c {
    () => {
        for ! in in {}
    };
}

fn main() {}
