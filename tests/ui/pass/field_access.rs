#[allow(unused_macros)]
#[expandable::expr]
macro_rules! field {
    ($e:expr, $f:ident) => {
        $e.$f
    };

    () => {
        "foo".bar + 1
    };

    () => {
        "foo".0 + 1
    };
}

fn main() {}
