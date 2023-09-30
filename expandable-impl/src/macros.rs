// Architecture invariant: this module contains macros that are used across the
// crate.

#[cfg(test)]
macro_rules! quote {
    (@inner $sb:expr, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::parenthesed($sb.mk_span(), quote! { @with_sb $sb, $( $tt )* })
    };


    (@inner $sb:expr, { $( $tt:tt )* } ) => {
        $crate::TokenTree::curlyBraced($sb.mk_span(), quote! { @with_sb $sb, $( $tt )* })
    };

    (@inner $sb:expr, @) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Dollar)
    };

    (@inner $sb:expr, :) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Colon)
    };

    (@inner $sb:expr, ?) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::QuestionMark)
    };

    (@inner $sb:expr, +) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Plus)
    };

    (@inner $sb:expr, *) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Times)
    };

    (@inner $sb:expr, =>) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::FatArrow)
    };

    (@inner $sb:expr, ;) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Semi)
    };

    // Keywords
    (@inner $sb:expr, as) => {
        $crate::TokenTree::terminal($sb.make_span(), $crate::Terminal::As)
    };

    (@inner $sb:expr, break) => {
        $crate::TokenTree::terminal($sb.make_span(), $crate::Terminal::Break)
    };

    (@inner $sb:expr, const) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Const)
    };

    (@inner $sb:expr, continue) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Continue)
    };

    (@inner $sb:expr, crate) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Crate)
    };

    (@inner $sb:expr, else) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Else)
    };

    (@inner $sb:expr, enum) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Enum)
    };

    (@inner $sb:expr, extern) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Extern)
    };

    (@inner $sb:expr, false) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::False)
    };

    (@inner $sb:expr, fn) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Fn)
    };

    (@inner $sb:expr, for) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::For)
    };

    (@inner $sb:expr, if) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::If)
    };

    (@inner $sb:expr, impl) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Impl)
    };

    (@inner $sb:expr, in) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::In)
    };

    (@inner $sb:expr, let) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Let)
    };

    (@inner $sb:expr, loop) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Loop)
    };

    (@inner $sb:expr, match) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Match)
    };

    (@inner $sb:expr, mod) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Mod)
    };

    (@inner $sb:expr, move) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Move)
    };

    (@inner $sb:expr, mut) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Mut)
    };

    (@inner $sb:expr, pub) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Pub)
    };

    (@inner $sb:expr, ref) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Ref)
    };

    (@inner $sb:expr, return) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Return)
    };

    (@inner $sb:expr, self) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Self_)
    };

    (@inner $sb:expr, Self) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::SelfUpper)
    };

    (@inner $sb:expr, static) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Static)
    };

    (@inner $sb:expr, struct) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Struct)
    };

    (@inner $sb:expr, super) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Super)
    };

    (@inner $sb:expr, trait) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Trait)
    };

    (@inner $sb:expr, true) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::True)
    };

    (@inner $sb:expr, type) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Type)
    };

    (@inner $sb:expr, unsafe) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Unsafe)
    };

    (@inner $sb:expr, use) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Use)
    };

    (@inner $sb:expr, where) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Where)
    };

    (@inner $sb:expr, while) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::While)
    };

    // Keywords that are also reserved
    (@inner $sb:expr, abstract) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Abstract)
    };

    (@inner $sb:expr, alignof) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Alignof)
    };

    (@inner $sb:expr, become) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Become)
    };

    (@inner $sb:expr, box) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Box)
    };

    (@inner $sb:expr, do) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Do)
    };

    (@inner $sb:expr, final) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Final)
    };

    (@inner $sb:expr, macro) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Macro)
    };

    (@inner $sb:expr, offsetof) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Offsetof)
    };

    (@inner $sb:expr, override) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Override)
    };

    (@inner $sb:expr, priv) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Priv)
    };

    (@inner $sb:expr, proc) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Proc)
    };

    (@inner $sb:expr, pure) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Pure)
    };

    (@inner $sb:expr, sizeof) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Sizeof)
    };

    (@inner $sb:expr, typeof) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Typeof)
    };

    (@inner $sb:expr, unsized) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Unsized)
    };

    (@inner $sb:expr, virtual) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Virtual)
    };

    (@inner $sb:expr, yield) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Yield)
    };

    (@inner $sb:expr, await) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Await)
    };

    (@inner $sb:expr, dyn) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Dyn)
    };

    (@inner $sb:expr, abstract) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Abstract)
    };

    (@inner $sb:expr, catch) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Catch)
    };

    (@inner $sb:expr, final) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Final)
    };

    (@inner $sb:expr, macro) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Macro)
    };

    (@inner $sb:expr, override) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Override)
    };

    (@inner $sb:expr, priv) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Priv)
    };

    (@inner $sb:expr, try) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Try)
    };

    (@inner $sb:expr, union) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Union)
    };

    (@inner $sb:expr, #) => {
        $crate::TokenDescription::Pound
    };

    (@inner $sb:expr, $id:ident) => {
        $crate::TokenTree::terminal($sb.mk_span(), $crate::Terminal::Ident(stringify!($id).to_string()))
    };

    (@with_sb $sb:expr, $( $tt:tt )* ) => {{
        vec![
            $(
                quote!(@inner $sb, $tt)
            ),*
        ]
    }};

    ( $( $tt:tt )* ) => {{
        #[allow(unused_variables, unused_mut)]
        let mut span_builder = $crate::span::DebugSpanBuilder::new();
        quote! { @with_sb span_builder, $( $tt )* }
    }};
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
                        span: 0,
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "b",
                            ),
                        ),
                        span: 1,
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "c",
                            ),
                        ),
                        span: 2,
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "d",
                            ),
                        ),
                        span: 3,
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
                        span: 0,
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: 1,
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "b",
                            ),
                        ),
                        span: 2,
                    },
                    TokenTree {
                        kind: Terminal(
                            Dollar,
                        ),
                        span: 3,
                    },
                    TokenTree {
                        kind: Parenthesed(
                            [
                                TokenTree {
                                    kind: Terminal(
                                        Dollar,
                                    ),
                                    span: 5,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "test",
                                        ),
                                    ),
                                    span: 6,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Colon,
                                    ),
                                    span: 7,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "ident",
                                        ),
                                    ),
                                    span: 8,
                                },
                            ],
                        ),
                        span: 4,
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: 9,
                    },
                    TokenTree {
                        kind: Terminal(
                            Times,
                        ),
                        span: 10,
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
                        span: 0,
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: 1,
                    },
                    TokenTree {
                        kind: Terminal(
                            Ident(
                                "b",
                            ),
                        ),
                        span: 2,
                    },
                    TokenTree {
                        kind: Terminal(
                            Dollar,
                        ),
                        span: 3,
                    },
                    TokenTree {
                        kind: Parenthesed(
                            [
                                TokenTree {
                                    kind: Terminal(
                                        Plus,
                                    ),
                                    span: 5,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Dollar,
                                    ),
                                    span: 6,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "c",
                                        ),
                                    ),
                                    span: 7,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Plus,
                                    ),
                                    span: 8,
                                },
                                TokenTree {
                                    kind: Terminal(
                                        Ident(
                                            "a",
                                        ),
                                    ),
                                    span: 9,
                                },
                            ],
                        ),
                        span: 4,
                    },
                    TokenTree {
                        kind: Terminal(
                            Plus,
                        ),
                        span: 10,
                    },
                    TokenTree {
                        kind: Terminal(
                            QuestionMark,
                        ),
                        span: 11,
                    },
                ]
            "#]],
        }
    }
}
