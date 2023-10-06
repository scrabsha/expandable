#[allow(unused_macros)]
#[expandable::item]
macro_rules! factorial {
    (fn $name:ident($input:ident) -> usize) => {
        fn $name($input: usize) -> usize {
            if $input == 0 {
                1
            } else {
                $input * $name($input - 1)
            }
        }
    };
}

fn main() {}
