#[allow(unused)]
#[expandable::item]
macro_rules! test {
    (
        $block:block,
        $expr:expr,
        $ident:ident,
        $lifetime:lifetime,
        // TODO: currently not handled
        $meta:meta,
        $pat:pat,
        $path:path,
        $pat_param:pat_param,
        $stmt:stmt,
        $tt:tt,
        $ty:ty,
        $vis:vis
    ) => {
        $vis fn $ident($pat: $ty, $pat_param: $ty) {
            $block;

            $lifetime loop {
                $stmt
            }

            let $pat = $block;
            let ($pat_param) = $expr;
            $path! { $tt }
        }
    }
}

fn main() {}
