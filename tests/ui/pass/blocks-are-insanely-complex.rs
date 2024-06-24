#[allow(unused)]
#[expandable::expr]
macro_rules! test {
    ($e:expr, $i:ident) => {{
        let a = 42;
        let $i = 42;
        let a: path::to::somewhere = ();
        let a: macro::call!() = ();
        let a = macro_call!();
        let a = macro_call!() + macro_call!();
        let a = macro_call!() + (anything, actually);
        just.an.expression;
        macro_call!();
        macro_call! {}
        macro_call!() + 42;
        macro_call! {} + 42;
        ;
        ;
        ;
        ;
        foo + bar
    }};
}

fn main() {}
