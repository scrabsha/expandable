#[allow(unused_macros)]
#[expandable::expr]
macro_rules! if_ {
    () => {
        0..42
    };

    () => {
        f(a)..=g(b)
    };
}

fn main() {}
