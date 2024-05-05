#![allow(unused)]

#[expandable::expr]
macro_rules! test {
    () => {{
        let a = ;
    }}
}

fn main() {}
