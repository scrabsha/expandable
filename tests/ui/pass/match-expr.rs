#[allow(unused)]
#[expandable::expr]
#[rustfmt::skip]
macro_rules! test {
    ($e:expr, $p:pat) => {
        match $e {
            // TODO(scrabsha): add tests for match arms that don't end with `,`.
            $p => $e,
            () => {},
            () if a != $e => {},
            _ => 42,
            _ | _ => 31,
        }
    };
}

fn main() {}
