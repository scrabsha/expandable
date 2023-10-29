#[allow(unused_macros)]
#[expandable::expr]
macro_rules! mk_array {
    ( $( $e:expr ),* $(,)? ) => {
        [ $( $e ),* ]
    };
}

fn main() {}
