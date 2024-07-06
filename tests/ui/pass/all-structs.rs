#[allow(unused)]
#[expandable::item]
#[rustfmt::skip]
macro_rules! test {
    ($id:ident) => {
        struct Foo;
        struct Foo {}
        struct Foo();

        struct $id;

        struct Bar {
            bazq: u8,
        }
        struct Bar {
            baz: u8
        }
        struct Bar(u8,);
        struct Bar(u8);

        pub struct Baz {
            pub baz: u8,
            pub(crate) baz: u16,
        }
        struct Baz(pub u8, pub foo::bar!());
    };

    // TODO: support the ty fragment kind.
    ($name:ident $( $field:ident: $ty:ident ),* $(,)? ) => {
        struct $name {
            $(
                $field: $ty,
            )*
        }
    };
}

fn main() {}
