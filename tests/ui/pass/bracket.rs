#[allow(unused_macros)]
#[expandable::expr]
macro_rules! mk_array {
    ( $( $e:expr ),* $(,)? ) => {
        [ $( $e ),* ]
    };

    ( $repeat:expr; $size:expr ) => {
        [ $repeat; $size ]
    };
}

fn main() {}
