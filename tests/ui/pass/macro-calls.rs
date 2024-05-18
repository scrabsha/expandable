#![allow(unused)]

#[expandable::expr]
macro_rules! a {
    () => {{
        foo!();
        foo::bar::baz!();
        foo!([], {}{});
    }};
}

#[expandable::expr]
macro_rules! b {
    () => {{
        let _: foo!() = ();
        let _: bar![] = ();
    }};
}

#[expandable::expr]
macro_rules! c {
    () => {{
        let foo!() = ();
        let ::foo::bar::baz![{}] = ();
    }};
}

fn main() {}
