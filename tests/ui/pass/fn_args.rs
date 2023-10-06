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
}

fn main() {}
