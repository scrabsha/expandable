#[allow(unused_macros)]
#[expandable::item]
macro_rules! test {
    () => {
        fn test() -> u8 {
            42
        }
    };

    () => {
        fn test(a: u8) -> u8 {
            42
        }
    };

    () => {
        fn test(a: u8) -> u8 {
            42
        }
    };

    () => {
        fn test(a: u8) -> u8 {
            42;
            101;
            42 + 101
        }
    };

    () => {
        fn test() -> u8 {
            { 42 }
        }
    };

    () => {
        fn test() -> u8 {
            let x = 1;
            let y = x + 1;
            let z = y + 1;
            z + 1
        }
    };
}

fn main() {}
