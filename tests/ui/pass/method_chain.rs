#[allow(unused_macros)]
#[expandable::expr]
macro_rules! chain {
    () => {
        foo.bar
            .baz()
            .qux
            .await
            .unwrap()
            .conv::<u32>()
            .conv::<u32>()
            .conv::<u32, u32>()
            .conv::<u32, u32, u32>()
            .19
    };
}

fn main() {}
