#![allow(unused)]

#[expandable::expr]
macro_rules! a {
    () => {
        a[b]
    };

    ($a:expr, $b:expr) => {
        $a[$b]
    };
}

fn main() {}
