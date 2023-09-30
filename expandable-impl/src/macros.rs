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

    (@mk_term $sb:expr, $term:expr) => {
        $crate::TokenTree::terminal($sb.mk_span(), $term)
    };

    (@inner $sb:expr, @) => {
        quote!(@mk_term $sb, $crate::Terminal::Dollar)
    };

    (@inner $sb:expr, :) => {
        quote!(@mk_term $sb, $crate::Terminal::Colon)
    };

    (@inner $sb:expr, ?) => {
        quote!(@mk_term $sb, $crate::Terminal::QuestionMark)
    };

    (@inner $sb:expr, +) => {
        quote!(@mk_term $sb, $crate::Terminal::Plus)
    };

    (@inner $sb:expr, *) => {
        quote!(@mk_term $sb, $crate::Terminal::Times)
    };

    (@inner $sb:expr, =>) => {
        quote!(@mk_term $sb, $crate::Terminal::FatArrow)
    };

    (@inner $sb:expr, ;) => {
        quote!(@mk_term $sb, $crate::Terminal::Semi)
    };

    // Keywords
    (@inner $sb:expr, as) => {
        quote!(@mk_term $sb, $crate::Terminal::As)
    };

    (@inner $sb:expr, break) => {
        quote!(@mk_term $sb, $crate::Terminal::Break)
    };

    (@inner $sb:expr, const) => {
        quote!(@mk_term $sb, $crate::Terminal::Const)
    };

    (@inner $sb:expr, continue) => {
        quote!(@mk_term $sb, $crate::Terminal::Continue)
    };

    (@inner $sb:expr, crate) => {
        quote!(@mk_term $sb, $crate::Terminal::Crate)
    };

    (@inner $sb:expr, else) => {
        quote!(@mk_term $sb, $crate::Terminal::Else)
    };

    (@inner $sb:expr, enum) => {
        quote!(@mk_term $sb, $crate::Terminal::Enum)
    };

    (@inner $sb:expr, extern) => {
        quote!(@mk_term $sb, $crate::Terminal::Extern)
    };

    (@inner $sb:expr, false) => {
        quote!(@mk_term $sb, $crate::Terminal::False)
    };

    (@inner $sb:expr, fn) => {
        quote!(@mk_term $sb, $crate::Terminal::Fn)
    };

    (@inner $sb:expr, for) => {
        quote!(@mk_term $sb, $crate::Terminal::For)
    };

    (@inner $sb:expr, if) => {
        quote!(@mk_term $sb, $crate::Terminal::If)
    };

    (@inner $sb:expr, impl) => {
        quote!(@mk_term $sb, $crate::Terminal::Impl)
    };

    (@inner $sb:expr, in) => {
        quote!(@mk_term $sb, $crate::Terminal::In)
    };

    (@inner $sb:expr, let) => {
        quote!(@mk_term $sb, $crate::Terminal::Let)
    };

    (@inner $sb:expr, loop) => {
        quote!(@mk_term $sb, $crate::Terminal::Loop)
    };

    (@inner $sb:expr, match) => {
        quote!(@mk_term $sb, $crate::Terminal::Match)
    };

    (@inner $sb:expr, mod) => {
        quote!(@mk_term $sb, $crate::Terminal::Mod)
    };

    (@inner $sb:expr, move) => {
        quote!(@mk_term $sb, $crate::Terminal::Move)
    };

    (@inner $sb:expr, mut) => {
        quote!(@mk_term $sb, $crate::Terminal::Mut)
    };

    (@inner $sb:expr, pub) => {
        quote!(@mk_term $sb, $crate::Terminal::Pub)
    };

    (@inner $sb:expr, ref) => {
        quote!(@mk_term $sb, $crate::Terminal::Ref)
    };

    (@inner $sb:expr, return) => {
        quote!(@mk_term $sb, $crate::Terminal::Return)
    };

    (@inner $sb:expr, self) => {
        quote!(@mk_term $sb, $crate::Terminal::Self_)
    };

    (@inner $sb:expr, Self) => {
        quote!(@mk_term $sb, $crate::Terminal::SelfType)
    };

    (@inner $sb:expr, static) => {
        quote!(@mk_term $sb, $crate::Terminal::Static)
    };

    (@inner $sb:expr, struct) => {
        quote!(@mk_term $sb, $crate::Terminal::Struct)
    };

    (@inner $sb:expr, super) => {
        quote!(@mk_term $sb, $crate::Terminal::Super)
    };

    (@inner $sb:expr, trait) => {
        quote!(@mk_term $sb, $crate::Terminal::Trait)
    };

    (@inner $sb:expr, true) => {
        quote!(@mk_term $sb, $crate::Terminal::True)
    };

    (@inner $sb:expr, type) => {
        quote!(@mk_term $sb, $crate::Terminal::Type)
    };

    (@inner $sb:expr, unsafe) => {
        quote!(@mk_term $sb, $crate::Terminal::Unsafe)
    };

    (@inner $sb:expr, use) => {
        quote!(@mk_term $sb, $crate::Terminal::Use)
    };

    (@inner $sb:expr, where) => {
        quote!(@mk_term $sb, $crate::Terminal::Where)
    };

    (@inner $sb:expr, while) => {
        quote!(@mk_term $sb, $crate::Terminal::While)
    };

    // Keywords that are also reserved
    (@inner $sb:expr, abstract) => {
        quote!(@mk_term $sb, $crate::Terminal::Abstract)
    };

    (@inner $sb:expr, alignof) => {
        quote!(@mk_term $sb, $crate::Terminal::Alignof)
    };

    (@inner $sb:expr, become) => {
        quote!(@mk_term $sb, $crate::Terminal::Become)
    };

    (@inner $sb:expr, box) => {
        quote!(@mk_term $sb, $crate::Terminal::Box)
    };

    (@inner $sb:expr, do) => {
        quote!(@mk_term $sb, $crate::Terminal::Do)
    };

    (@inner $sb:expr, final) => {
        quote!(@mk_term $sb, $crate::Terminal::Final)
    };

    (@inner $sb:expr, macro) => {
        quote!(@mk_term $sb, $crate::Terminal::Macro)
    };

    (@inner $sb:expr, offsetof) => {
        quote!(@mk_term $sb, $crate::Terminal::Offsetof)
    };

    (@inner $sb:expr, override) => {
        quote!(@mk_term $sb, $crate::Terminal::Override)
    };

    (@inner $sb:expr, priv) => {
        quote!(@mk_term $sb, $crate::Terminal::Priv)
    };

    (@inner $sb:expr, proc) => {
        quote!(@mk_term $sb, $crate::Terminal::Proc)
    };

    (@inner $sb:expr, pure) => {
        quote!(@mk_term $sb, $crate::Terminal::Pure)
    };

    (@inner $sb:expr, sizeof) => {
        quote!(@mk_term $sb, $crate::Terminal::Sizeof)
    };

    (@inner $sb:expr, typeof) => {
        quote!(@mk_term $sb, $crate::Terminal::Typeof)
    };

    (@inner $sb:expr, unsized) => {
        quote!(@mk_term $sb, $crate::Terminal::Unsized)
    };

    (@inner $sb:expr, virtual) => {
        quote!(@mk_term $sb, $crate::Terminal::Virtual)
    };

    (@inner $sb:expr, yield) => {
        quote!(@mk_term $sb, $crate::Terminal::Yield)
    };

    (@inner $sb:expr, await) => {
        quote!(@mk_term $sb, $crate::Terminal::Await)
    };

    (@inner $sb:expr, dyn) => {
        quote!(@mk_term $sb, $crate::Terminal::Dyn)
    };

    (@inner $sb:expr, abstract) => {
        quote!(@mk_term $sb, $crate::Terminal::Abstract)
    };

    (@inner $sb:expr, catch) => {
        quote!(@mk_term $sb, $crate::Terminal::Catch)
    };

    (@inner $sb:expr, final) => {
        quote!(@mk_term $sb, $crate::Terminal::Final)
    };

    (@inner $sb:expr, macro) => {
        quote!(@mk_term $sb, $crate::Terminal::Macro)
    };

    (@inner $sb:expr, override) => {
        quote!(@mk_term $sb, $crate::Terminal::Override)
    };

    (@inner $sb:expr, priv) => {
        quote!(@mk_term $sb, $crate::Terminal::Priv)
    };

    (@inner $sb:expr, try) => {
        quote!(@mk_term $sb, $crate::Terminal::Try)
    };

    (@inner $sb:expr, union) => {
        quote!(@mk_term $sb, $crate::Terminal::Union)
    };

    (@inner $sb:expr, #) => {
        quote!(@mk_term $sb, $crate::Terminal::Pound)
    };

    (@inner $sb:expr, $id:ident) => {
        quote!(@mk_term $sb, $crate::Terminal::Ident(stringify!($id).to_string()))
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
