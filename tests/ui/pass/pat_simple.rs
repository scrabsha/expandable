#![allow(unused)]

#[expandable::pat]
macro_rules! test {
    ($p:pat) => {
        $p
    };
}

fn main() {}
