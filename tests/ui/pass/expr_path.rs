#[allow(unused)]
#[expandable::expr]
macro_rules! test {
    ( $( $ident:ident )+ ) => {
        $( :: $ident )+
    };

    () => {
        ::a::<b>::c::<d>()
    }
}

fn main() {}
