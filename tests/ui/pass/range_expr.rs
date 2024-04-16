#[allow(unused_macros)]
#[expandable::expr]
macro_rules! range_expr {
    () => {
        0..42
    };

    () => {
        f(a)..=g(b)
    };
}

fn main() {}
