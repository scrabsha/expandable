// Architecture invariant: this module contains macros that are used across the
// crate.

/// Creates a sequence of [`TokenTree`]s from a sequence of tokens. Useful for
/// testing.
///
/// This macro can't parse the `$` token. It uses `#` instead.
///
/// [`TokenTree`]: crate::TokenTree
#[macro_export]
macro_rules! quote {
    (@mk_term $sb:expr, $term:expr $(,)?) => {
        $crate::TokenTree::terminal($sb.mk_span(), $term)
    };

    (@inner $sb:expr, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::parenthesed($sb.mk_span(), $crate::quote! { @with_sb $sb, $( $tt )* })
    };

    (@inner $sb:expr, { $( $tt:tt )* } ) => {
        $crate::TokenTree::curlyBraced($sb.mk_span(), $crate::quote! { @with_sb $sb, $( $tt )* })
    };

    // Keywords
    (@inner $sb:expr, as) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::As)
    };

    (@inner $sb:expr, async) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Async)
    };

    (@inner $sb:expr, break) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Break)
    };

    (@inner $sb:expr, const) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Const)
    };

    (@inner $sb:expr, continue) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Continue)
    };

    (@inner $sb:expr, crate) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Crate)
    };

    (@inner $sb:expr, else) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Else)
    };

    (@inner $sb:expr, enum) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Enum)
    };

    (@inner $sb:expr, extern) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Extern)
    };

    (@inner $sb:expr, false) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::False)
    };

    (@inner $sb:expr, fn) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Fn)
    };

    (@inner $sb:expr, for) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::For)
    };

    (@inner $sb:expr, if) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::If)
    };

    (@inner $sb:expr, impl) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Impl)
    };

    (@inner $sb:expr, in) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::In)
    };

    (@inner $sb:expr, let) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Let)
    };

    (@inner $sb:expr, loop) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Loop)
    };

    (@inner $sb:expr, match) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Match)
    };

    (@inner $sb:expr, mod) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Mod)
    };

    (@inner $sb:expr, move) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Move)
    };

    (@inner $sb:expr, mut) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Mut)
    };

    (@inner $sb:expr, pub) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Pub)
    };

    (@inner $sb:expr, ref) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Ref)
    };

    (@inner $sb:expr, return) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Return)
    };

    (@inner $sb:expr, self) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Self_)
    };

    (@inner $sb:expr, Self) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::SelfUpper)
    };

    (@inner $sb:expr, static) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Static)
    };

    (@inner $sb:expr, struct) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Struct)
    };

    (@inner $sb:expr, super) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Super)
    };

    (@inner $sb:expr, trait) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Trait)
    };

    (@inner $sb:expr, true) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::True)
    };

    (@inner $sb:expr, type) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Type)
    };

    (@inner $sb:expr, unsafe) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Unsafe)
    };

    (@inner $sb:expr, use) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Use)
    };

    (@inner $sb:expr, where) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Where)
    };

    (@inner $sb:expr, while) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::While)
    };

    // Keywords that are also reserved
    (@inner $sb:expr, abstract) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Abstract)
    };

    (@inner $sb:expr, become) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Become)
    };

    (@inner $sb:expr, box) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Box)
    };

    (@inner $sb:expr, do) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Do)
    };

    (@inner $sb:expr, final) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Final)
    };

    (@inner $sb:expr, macro) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Macro)
    };

    (@inner $sb:expr, offsetof) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Offsetof)
    };

    (@inner $sb:expr, override) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Override)
    };

    (@inner $sb:expr, priv) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Priv)
    };

    (@inner $sb:expr, proc) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Proc)
    };

    (@inner $sb:expr, sizeof) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Sizeof)
    };

    (@inner $sb:expr, typeof) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Typeof)
    };

    (@inner $sb:expr, unsized) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Unsized)
    };

    (@inner $sb:expr, virtual) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Virtual)
    };

    (@inner $sb:expr, yield) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Yield)
    };

    (@inner $sb:expr, await) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Await)
    };

    (@inner $sb:expr, dyn) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Dyn)
    };

    (@inner $sb:expr, abstract) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Abstract)
    };

    (@inner $sb:expr, catch) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Catch)
    };

    (@inner $sb:expr, final) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Final)
    };

    (@inner $sb:expr, macro) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Macro)
    };

    (@inner $sb:expr, override) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Override)
    };

    (@inner $sb:expr, priv) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Priv)
    };

    (@inner $sb:expr, try) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Try)
    };

    (@inner $sb:expr, union) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Union)
    };

    (@inner $sb:expr, $id:ident) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Ident(stringify!($id).to_string()))
    };

    // Punctuates
    (@inner $sb:expr, +) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Plus)
    };

    (@inner $sb:expr, -) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Minus)
    };

    (@inner $sb:expr, *) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Star)
    };

    (@inner $sb:expr, /) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Slash)
    };

    (@inner $sb:expr, %) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Percent)
    };

    (@inner $sb:expr, ^) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Caret)
    };

    (@inner $sb:expr, !) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Not)
    };

    (@inner $sb:expr, &) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::And)
    };

    (@inner $sb:expr, |) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Or)
    };

    (@inner $sb:expr, &&) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::AndAnd)
    };

    (@inner $sb:expr, ||) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::OrOr)
    };

    (@inner $sb:expr, <<) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Shl)
    };

    (@inner $sb:expr, >>) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Shr)
    };

    (@inner $sb:expr, +=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::PlusEquals)
    };

    (@inner $sb:expr, -=) => {
       $crate::quote!(@mk_term $sb, $crate::Terminal::MinusEquals)
    };

    (@inner $sb:expr, *=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::StarEquals)
    };

    (@inner $sb:expr, /=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::SlashEquals)
    };

    (@inner $sb:expr, %=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::PercentEquals)
    };

    (@inner $sb:expr, ^=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::CaretEquals)
    };

    (@inner $sb:expr, &=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::AndEquals)
    };

    (@inner $sb:expr, |=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::OrEquals)
    };

    (@inner $sb:expr, <<=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::ShlEquals)
    };

    (@inner $sb:expr, >>=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::ShrEquals)
    };

    (@inner $sb:expr, =) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Equals)
    };

    (@inner $sb:expr, ==) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::EqualsEquals)
    };

    (@inner $sb:expr, !=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::NotEquals)
    };

    (@inner $sb:expr, >) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::GreaterThan)
    };

    (@inner $sb:expr, <) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::LessThan)
    };

    (@inner $sb:expr, >=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::GreaterThanEquals)
    };

    (@inner $sb:expr, <=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::LessThanEquals)
    };

    (@inner $sb:expr, @) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::At)
    };

    (@inner $sb:expr, _) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Underscore)
    };

    (@inner $sb:expr, .) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Dot)
    };

    (@inner $sb:expr, ..) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::DotDot)
    };

    (@inner $sb:expr, ...) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::DotDotDot)
    };

    (@inner $sb:expr, ..=) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::DotDotEquals)
    };

    (@inner $sb:expr, ,) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Comma)
    };

    (@inner $sb:expr, ; ) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Semicolon)
    };

    (@inner $sb:expr, : ) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Colon)
    };

    (@inner $sb:expr, ::) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::ColonColon)
    };

    (@inner $sb:expr, ->) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::RightArrow)
    };

    (@inner $sb:expr, =>) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::FatArrow)
    };

    // Warning: this is equivalent to the `$` sign.
    (@inner $sb:expr, #) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::Dollar)
    };

    (@inner $sb:expr, ?) => {
        $crate::quote!(@mk_term $sb, $crate::Terminal::QuestionMark)
    };

    // Keep this rule at the end of the list, so that it does not match `true`
    // and `false`, which are keywords.
    (@inner $sb:expr, $lit:literal) => {
        $crate::quote!(
            @mk_term $sb,
            $crate::Terminal::Literal(stringify!($lit).to_string()),
        )
    };

    (@inner $sb:expr, $tt:tt) => {
        compile_error!(concat!("Unexpected token: ", stringify!($tt)))
    };

    (@with_sb $sb:expr, ) => {
        vec![]
    };

    (@with_sb $sb:expr, $( $tt:tt )* ) => {{
        vec![
            $(
                $crate::quote!(@inner $sb, $tt)
            ),*
        ]
    }};

    (@inner $sb:expr, $tt:tt) => {
        compile_error!(concat!("Unexpected token: ", stringify!($tt)))
    };

    (@with_sb $sb:expr, ) => {
        vec![]
    };

    (@with_sb $sb:expr, $( $tt:tt )* ) => {{
        vec![
            $(
                $crate::quote!(@inner $sb, $tt)
            ),*
        ]
    }};

    ( $( $tt:tt )* ) => {{
        #[allow(unused_variables, unused_mut)]
        let mut span_builder = $crate::span::DebugSpanBuilder::new();
        $crate::quote! { @with_sb span_builder, $( $tt )* }
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
            { a + b #( #test:ident )+* },
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
                            Star,
                        ),
                        span: 10,
                    },
                ]
            "#]],
        }
    }

    test_quote! {
        can_parse_substition_syntax {
            { a + b #( + #c + a )+? },
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
