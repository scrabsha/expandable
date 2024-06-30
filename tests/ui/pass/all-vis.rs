#[allow(unused)]
#[expandable::item]
macro_rules! test {
    (p: path) => {
        fn foo() {}
        pub fn bar() {}
        pub(crate) fn baz() {}
        pub(super) fn qux() {}
        pub(self) fn quux() {}
        pub(in foo::bar::baz) fn quuux() {}
        pub(in ::foo::bar::baz) fn quuux() {}
    };
}

fn main() {}
