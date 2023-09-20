#[allow(unused_macros)]
#[expandable::expr]
macro_rules! mac {
    () => {
        a + b + c
    };
    () => {
        a * b * c
    };
    () => {
        a + b * c
    };
}

fn main() {}
