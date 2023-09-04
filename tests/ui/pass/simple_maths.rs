#[allow(unused_macros)]
#[expandable::expandable(expr)]
macro_rules! mac {
    () => { a + b + c };
    () => { a * b * c };
    () => { a + b * c };
}

fn main() {}
