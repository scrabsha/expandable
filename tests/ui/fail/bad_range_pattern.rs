#![allow(unused)]

#[expandable::expr]
macro_rules! t {
    ($p:pat) => {{
        let $p..=0 = ();
    }};
}

fn main() {}
