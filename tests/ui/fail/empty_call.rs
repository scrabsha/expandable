// This should fail because "no tokens" is not a valid expression.

#[expandable::expandable(expr)]
macro_rules! empty {
    () => {}
}

fn main() {}