#[allow(unused)]
#[expandable::item]
macro_rules! test {
    () => {
        struct S(pub (u32));
    };
}

fn main() {}
