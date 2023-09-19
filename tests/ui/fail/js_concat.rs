#[allow(unused_macros)]
#[expandable::expr]
macro_rules! js_concat {
    ($left:expr, $right:expr) => {
        $left ++ $right
    };
}

fn main() {}
