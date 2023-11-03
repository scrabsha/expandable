#[allow(unused_macros)]
#[expandable::expr]
macro_rules! r#await {
    ($e:expr) => {
        $e.await
    };
}

fn main() {}
