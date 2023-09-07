#[expandable::expr]
macro_rules! square {
    ($e:expr) => { $e ** $e }
}

fn main() {}