#[allow(unused_macros)]
#[expandable::expr]
macro_rules! r#break {
    ($e:expr) => {
        break $e
    };

    ($e:expr) => {
        break break break break break break break 42
    };

    () => {
        break
    };
}

fn main() {}
