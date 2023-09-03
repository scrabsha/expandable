use expandable::expandable;

#[expandable(expr)]
macro_rules! __ {
    () => {};
}

fn main() {
    let a = __!();
}