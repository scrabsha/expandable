#[expandable::expr]
macro_rules! bad_nesting_example {
    ( $( $a:ident )? ) => { $( $a )* }
}

fn main() {}
