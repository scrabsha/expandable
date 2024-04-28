#![allow(unused)]

#[expandable::expr]
macro_rules! a {
    () => {
        (42)
    };
}

#[expandable::expr]
macro_rules! b {
    ($e:expr) => {
        ($e)
    };
}

#[expandable::expr]
macro_rules! c {
    ($($e:expr $(, $e_:expr)* $(,)?)?) => {
        ( $( $e $(, $e_)*)?)
    };
}

fn main() {}
