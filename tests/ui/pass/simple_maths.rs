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
    ($( $d:expr, )* ) => {
        a - b % c $( + $d )*
    };
    // Bit expressions
    () => {
        a & b | c ^ d << e >> f
    };
    // Comparison expressions
    () => {
        a == b != c < d > e <= f >= g
    };
}

fn main() {}
