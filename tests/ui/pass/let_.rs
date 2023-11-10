#[allow(unused_macros)]
#[expandable::expr]
macro_rules! let_ {
    ($x:ident = $y:expr;) => {{
        let $x = $y;
        $x
    }};

    () => {{
        let x = 1;
        let y = x;
        y
    }};
}

fn main() {}
