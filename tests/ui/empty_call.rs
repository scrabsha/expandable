use expandable::expandable;

#[expandable(expr)]
macro_rules! empty {
    () => {}
}

fn main() {}