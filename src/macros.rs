macro_rules! quote {
    (@inner, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::Parenthesed(quote! { $( $tt )* })
    };

    (@inner, { $( $tt:tt )* } ) => {
        $crate::TokenTree::CurlyBraced(quote! { $( $tt )* })
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
                quote!(@inner, $tt)
            ),*
        ]
    };
}

mod macros_ {
    macro_rules! test_quote {
        ($name:ident {
            { $( $left:tt )* },
            $right:expr $(,)?
        }) => {
            #[test]
            fn $name() {
                let left = quote! { $( $left )* };
                let right = $right;

                right.assert_debug_eq(&left);
            }
        }
    }

    test_quote! {
        simple_traduction {
            { a b c d },
            expect_test::expect![[r#"
                [
                    Terminal(
                        Ident(
                            "a",
                        ),
                    ),
                    Terminal(
                        Ident(
                            "b",
                        ),
                    ),
                    Terminal(
                        Ident(
                            "c",
                        ),
                    ),
                    Terminal(
                        Ident(
                            "d",
                        ),
                    ),
                ]
            "#]],
        }
    }

    test_quote! {
        handles_matcher_syntax {
            { a + b @( @test:ident )+* },
            expect_test::expect![[r#"
                [
                    Terminal(
                        Ident(
                            "a",
                        ),
                    ),
                    Terminal(
                        Plus,
                    ),
                    Terminal(
                        Ident(
                            "b",
                        ),
                    ),
                    Terminal(
                        Dollar,
                    ),
                    Parenthesed(
                        [
                            Terminal(
                                Dollar,
                            ),
                            Terminal(
                                Ident(
                                    "test",
                                ),
                            ),
                            Terminal(
                                Colon,
                            ),
                            Terminal(
                                Ident(
                                    "ident",
                                ),
                            ),
                        ],
                    ),
                    Terminal(
                        Plus,
                    ),
                    Terminal(
                        Times,
                    ),
                ]
            "#]],
        }
    }

    test_quote! {
        can_parse_substition_syntax {
            { a + b @( + @c + a )+? },
            expect_test::expect![[r#"
                [
                    Terminal(
                        Ident(
                            "a",
                        ),
                    ),
                    Terminal(
                        Plus,
                    ),
                    Terminal(
                        Ident(
                            "b",
                        ),
                    ),
                    Terminal(
                        Dollar,
                    ),
                    Parenthesed(
                        [
                            Terminal(
                                Plus,
                            ),
                            Terminal(
                                Dollar,
                            ),
                            Terminal(
                                Ident(
                                    "c",
                                ),
                            ),
                            Terminal(
                                Plus,
                            ),
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                    ),
                    Terminal(
                        Plus,
                    ),
                    Terminal(
                        QuestionMark,
                    ),
                ]
            "#]],
        }
    }
}
