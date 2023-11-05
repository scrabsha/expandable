#[allow(unused_macros)]
#[expandable::expr]
macro_rules! block {
    ($e:expr) => {{ $e }};

    () => {
        { 1 } + { 2 } == { 3 }
    }
}

fn main() {}
