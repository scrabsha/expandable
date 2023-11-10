#[allow(unused_macros)]
#[expandable::expr]
macro_rules! r#return {
    ($e:expr) => {
        return $e
    };

    ($e:expr) => {
        return return return return return return return 42
    };

    () => {
        return
    };
}

fn main() {}
