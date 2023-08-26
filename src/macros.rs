macro_rules! token_tree {
    (@inner, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::Parenthesed(token_tree! { $( $tt )* })
    };

    (@inner, { $( $tt:tt )* } ) => {
        $crate::TokenTree::CurlyBraced(token_tree! { $( $tt )* })
    };

    (@inner, $id:ident) => {
        $crate::TokenTree::Terminal($crate::Terminal::Ident(stringify!($id).to_string()))
    };

    (@inner, fn) => {
        $crate::TokenTree::Terminal($crate::Terminal::Ident("fn".to_string()))
    };

    (@inner, @) => {
        $crate::TokenTree::Terminal($crate::Terminal::Dollar)
    };

    (@inner, :) => {
        $crate::TokenTree::Terminal($crate::Terminal::Colon)
    };

    (@inner, ?) => {
        $crate::TokenTree::Terminal($crate::Terminal::QuestionMark)
    };

    (@inner, +) => {
        $crate::TokenTree::Terminal($crate::Terminal::Plus)
    };

    (@inner, *) => {
        $crate::TokenTree::Terminal($crate::Terminal::Times)
    };

    ( $( $tt:tt )* ) => {
        vec![
            $(
                token_tree!(@inner, $tt)
            ),*
        ]
    };
}
