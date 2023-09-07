// This should fail because "no tokens" is not a valid expression.

#[allow(unused_macros)]
#[expandable::expr]
macro_rules! empty {
    () => {}
}

fn main() {}