#[cfg(test)]
macro_rules! quote {
    (@inner, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::Parenthesed(quote! { $( $tt )* })
    };

    (@inner, { $( $tt:tt )* } ) => {
        $crate::TokenTree::CurlyBraced(quote! { $( $tt )* })
    };

    (@inner, fn) => {
        $crate::TokenTree::Terminal($crate::Terminal::Fn)
    };

    (@inner, $id:ident) => {
        $crate::TokenTree::Terminal($crate::Terminal::Ident(stringify!($id).to_string()))
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

    (@inner, =>) => {
        $crate::TokenTree::Terminal($crate::Terminal::FatArrow)
    };

    (@inner, ;) => {
        $crate::TokenTree::Terminal($crate::Terminal::Semi)
    };

    ( $( $tt:tt )* ) => {
        vec![
            $(
                quote!(@inner, $tt)
            ),*
        ]
    };
}

macro_rules! impl_spannable {
    ($in:ident $( <$span:ident> )? => $out:ident) => {
        impl<Span> $crate::Spannable<Span> for $in$(<$span>)? {
            type Output = $out<Span>;

            fn with_span(self, span: Span) -> $out<Span> {
                $out { kind: self.into(), span }
            }
        }
    };
}

mod tests {
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
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "a",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "b",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "c",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "d",
                            ),
                        ),
                        span: (),
                    },
                ]
            "#]],
        }
    }

    test_quote! {
        handles_matcher_syntax {
            { a + b @( @test:ident )+* },
            expect_test::expect![[r#"
                [
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "a",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "b",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Dollar,
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Parenthesed(
                            [
                                TokenTree {
                                    kind: Terminal(
                                        Dollar,
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "test",
                                        ),
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Colon,
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "ident",
                                        ),
                                    ),
                                    span: (),
                                },
                            ],
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Times,
                        ),
                        span: (),
                    },
                ]
            "#]],
        }
    }

    test_quote! {
        can_parse_substition_syntax {
            { a + b @( + @c + a )+? },
            expect_test::expect![[r#"
                [
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "a",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "b",
                            ),
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Dollar,
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Parenthesed(
                            [
                                TokenTree {
                                    kind: Terminal(
                                        Plus,
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Dollar,
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "c",
                                        ),
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Plus,
                                    ),
                                    span: (),
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "a",
                                        ),
                                    ),
                                    span: (),
                                },
                            ],
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: (),
                    },
                    TokenTree {
                        kind: Terminal(
                            QuestionMark,
                        ),
                        span: (),
                    },
                ]
            "#]],
        }
    }
}
