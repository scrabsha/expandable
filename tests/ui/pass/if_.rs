#[allow(unused_macros)]
#[expandable::expr]
macro_rules! if_ {
    () => {
        if a {
            b
        }
    };

    () => {
        if a {
            a
        } else {
            a
        }
    };

    ($a:ident) => {
        if $a {
            $a
        } else {
            $a
        }
    };
}

fn main() {}
