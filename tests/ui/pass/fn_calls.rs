#[allow(unused_macros)]
#[expandable::expr]
macro_rules! call {
    ($fn_name:ident()) => {
        $fn_name()
    };

    ($fn_name:ident($($args:expr),+)) => {
        $fn_name($($args),+)
    };

    ($fn_name:ident($($args:expr,)+)) => {
        $fn_name($($args,)+)
    };

    () => {
        foo::<bar>() + foo::<bar,>() + foo::<bar, baz,>()
    };

    () => {
        foo::<{bar}, -1, 1, foo>()
    };
}

fn main() {}
