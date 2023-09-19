// Architecture invariant: this module contains macros that are used across the
// crate.

#[cfg(test)]
macro_rules! quote {
    (@inner, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::Parenthesed(quote! { $( $tt )* })
    };

    (@inner, { $( $tt:tt )* } ) => {
        $crate::TokenTree::CurlyBraced(quote! { $( $tt )* })
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

    // Keywords
    (@inner, as) => {
        $crate::TokenTree::Terminal($crate::Terminal::As)
    };

    (@inner, break) => {
        $crate::TokenTree::Terminal($crate::Terminal::Break)
    };

    (@inner, const) => {
        $crate::TokenTree::Terminal($crate::Terminal::Const)
    };

    (@inner, continue) => {
        $crate::TokenTree::Terminal($crate::Terminal::Continue)
    };

    (@inner, crate) => {
        $crate::TokenTree::Terminal($crate::Terminal::Crate)
    };

    (@inner, else) => {
        $crate::TokenTree::Terminal($crate::Terminal::Else)
    };

    (@inner, enum) => {
        $crate::TokenTree::Terminal($crate::Terminal::Enum)
    };

    (@inner, extern) => {
        $crate::TokenTree::Terminal($crate::Terminal::Extern)
    };

    (@inner, false) => {
        $crate::TokenTree::Terminal($crate::Terminal::False)
    };

    (@inner, fn) => {
        $crate::TokenTree::Terminal($crate::Terminal::Fn)
    };

    (@inner, for) => {
        $crate::TokenTree::Terminal($crate::Terminal::For)
    };

    (@inner, if) => {
        $crate::TokenTree::Terminal($crate::Terminal::If)
    };

    (@inner, impl) => {
        $crate::TokenTree::Terminal($crate::Terminal::Impl)
    };

    (@inner, in) => {
        $crate::TokenTree::Terminal($crate::Terminal::In)
    };

    (@inner, let) => {
        $crate::TokenTree::Terminal($crate::Terminal::Let)
    };

    (@inner, loop) => {
        $crate::TokenTree::Terminal($crate::Terminal::Loop)
    };

    (@inner, match) => {
        $crate::TokenTree::Terminal($crate::Terminal::Match)
    };

    (@inner, mod) => {
        $crate::TokenTree::Terminal($crate::Terminal::Mod)
    };

    (@inner, move) => {
        $crate::TokenTree::Terminal($crate::Terminal::Move)
    };

    (@inner, mut) => {
        $crate::TokenTree::Terminal($crate::Terminal::Mut)
    };

    (@inner, pub) => {
        $crate::TokenTree::Terminal($crate::Terminal::Pub)
    };

    (@inner, ref) => {
        $crate::TokenTree::Terminal($crate::Terminal::Ref)
    };

    (@inner, return) => {
        $crate::TokenTree::Terminal($crate::Terminal::Return)
    };

    (@inner, self) => {
        $crate::TokenTree::Terminal($crate::Terminal::Self_)
    };

    (@inner, Self) => {
        $crate::TokenTree::Terminal($crate::Terminal::SelfUpper)
    };

    (@inner, static) => {
        $crate::TokenTree::Terminal($crate::Terminal::Static)
    };

    (@inner, struct) => {
        $crate::TokenTree::Terminal($crate::Terminal::Struct)
    };

    (@inner, super) => {
        $crate::TokenTree::Terminal($crate::Terminal::Super)
    };

    (@inner, trait) => {
        $crate::TokenTree::Terminal($crate::Terminal::Trait)
    };

    (@inner, true) => {
        $crate::TokenTree::Terminal($crate::Terminal::True)
    };

    (@inner, type) => {
        $crate::TokenTree::Terminal($crate::Terminal::Type)
    };

    (@inner, unsafe) => {
        $crate::TokenTree::Terminal($crate::Terminal::Unsafe)
    };

    (@inner, use) => {
        $crate::TokenTree::Terminal($crate::Terminal::Use)
    };

    (@inner, where) => {
        $crate::TokenTree::Terminal($crate::Terminal::Where)
    };

    (@inner, while) => {
        $crate::TokenTree::Terminal($crate::Terminal::While)
    };

    // Keywords that are also reserved
    (@inner, abstract) => {
        $crate::TokenTree::Terminal($crate::Terminal::Abstract)
    };

    (@inner, alignof) => {
        $crate::TokenTree::Terminal($crate::Terminal::Alignof)
    };

    (@inner, become) => {
        $crate::TokenTree::Terminal($crate::Terminal::Become)
    };

    (@inner, box) => {
        $crate::TokenTree::Terminal($crate::Terminal::Box)
    };

    (@inner, do) => {
        $crate::TokenTree::Terminal($crate::Terminal::Do)
    };

    (@inner, final) => {
        $crate::TokenTree::Terminal($crate::Terminal::Final)
    };

    (@inner, macro) => {
        $crate::TokenTree::Terminal($crate::Terminal::Macro)
    };

    (@inner, offsetof) => {
        $crate::TokenTree::Terminal($crate::Terminal::Offsetof)
    };

    (@inner, override) => {
        $crate::TokenTree::Terminal($crate::Terminal::Override)
    };

    (@inner, priv) => {
        $crate::TokenTree::Terminal($crate::Terminal::Priv)
    };

    (@inner, proc) => {
        $crate::TokenTree::Terminal($crate::Terminal::Proc)
    };

    (@inner, pure) => {
        $crate::TokenTree::Terminal($crate::Terminal::Pure)
    };

    (@inner, sizeof) => {
        $crate::TokenTree::Terminal($crate::Terminal::Sizeof)
    };

    (@inner, typeof) => {
        $crate::TokenTree::Terminal($crate::Terminal::Typeof)
    };

    (@inner, unsized) => {
        $crate::TokenTree::Terminal($crate::Terminal::Unsized)
    };

    (@inner, virtual) => {
        $crate::TokenTree::Terminal($crate::Terminal::Virtual)
    };

    (@inner, yield) => {
        $crate::TokenTree::Terminal($crate::Terminal::Yield)
    };

    (@inner, await) => {
        $crate::TokenTree::Terminal($crate::Terminal::Await)
    };

    (@inner, dyn) => {
        $crate::TokenTree::Terminal($crate::Terminal::Dyn)
    };

    (@inner, abstract) => {
        $crate::TokenTree::Terminal($crate::Terminal::Abstract)
    };

    (@inner, catch) => {
        $crate::TokenTree::Terminal($crate::Terminal::Catch)
    };

    (@inner, final) => {
        $crate::TokenTree::Terminal($crate::Terminal::Final)
    };

    (@inner, macro) => {
        $crate::TokenTree::Terminal($crate::Terminal::Macro)
    };

    (@inner, override) => {
        $crate::TokenTree::Terminal($crate::Terminal::Override)
    };

    (@inner, priv) => {
        $crate::TokenTree::Terminal($crate::Terminal::Priv)
    };

    (@inner, try) => {
        $crate::TokenTree::Terminal($crate::Terminal::Try)
    };

    (@inner, union) => {
        $crate::TokenTree::Terminal($crate::Terminal::Union)
    };

    (@inner, #) => {
        $crate::TokenDescription::Pound
    };

    (@inner, $id:ident) => {
        $crate::TokenTree::Terminal($crate::Terminal::Ident(stringify!($id).to_string()))
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
