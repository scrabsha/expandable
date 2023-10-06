#[allow(unused_macros)]
#[expandable::expr]
macro_rules! call {
    // No argument, just a comma
    ($fn_name:ident()) => {
        $fn_name(,)
    };
}

#[allow(unused_macros)]
#[expandable::expr]
macro_rules! call_ {
    ($fn_name:ident()) => {
        $fn_name(,,)
    };
}

#[allow(unused_macros)]
#[expandable::expr]
macro_rules! call2 {
    ($fn_name:ident($arg1:expr, $arg2:expr)) => {
        $fn_name($arg1 $arg2)
    };
}

fn main() {}
