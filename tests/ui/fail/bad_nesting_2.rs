#[expandable::expr]
#[allow(unused_macros)]
macro_rules! bad_nesting_example {
    ( $( $( $( $a:ident, )*, )+, )? ) => {
        $( $a )*
    };
}

fn main() {}
