#![allow(unused)]

#[expandable::expr]
macro_rules! a {
    () => {
        path:::<foo>
    };
}

#[expandable::expr]
macro_rules! b {
    () => {
        path::< <foo> >
    };
}

#[expandable::expr]
macro_rules! c {
    () => {
        path::1
    };
}

fn main() {}
