use std::collections::HashMap;

use crate::{FragmentKind, RepetitionQuantifier, Terminal, TokenTree as GenericTokenTree};

type Result<T> = std::result::Result<T, ()>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TokenTree {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree>),
    CurlyBraced(Vec<TokenTree>),
    Binding {
        name: String,
        kind: FragmentKind,
    },
    Repetition {
        inner: Vec<TokenTree>,
        separator: Option<Box<TokenTree>>,
        quantifier: RepetitionQuantifier,
    },
}

impl TokenTree {
    pub(crate) fn from_generic(generic: Vec<GenericTokenTree>) -> Result<Vec<TokenTree>> {
        let mut out = Vec::with_capacity(generic.len());
        let mut iter = generic.into_iter();

        while let Some(token) = iter.next() {
            let tok = match token {
                // $
                GenericTokenTree::Terminal(Terminal::Dollar) => match iter.next().ok_or(())? {
                    // $ident
                    GenericTokenTree::Terminal(Terminal::Ident(id)) => {
                        Self::parse_fragment(&mut iter, id)
                    }
                    // $(...)
                    GenericTokenTree::Parenthesed(inner) => {
                        Self::parse_repetition(&mut iter, inner)
                    }

                    GenericTokenTree::Terminal(_) | GenericTokenTree::CurlyBraced(_) => {
                        return Err(())
                    }
                }?,

                GenericTokenTree::Terminal(t) => TokenTree::Terminal(t),

                GenericTokenTree::Parenthesed(inner) => {
                    TokenTree::Parenthesed(Self::from_generic(inner)?)
                }

                GenericTokenTree::CurlyBraced(inner) => {
                    TokenTree::CurlyBraced(Self::from_generic(inner)?)
                }
            };

            out.push(tok);
        }

        Ok(out)
    }

    fn parse_fragment(
        iter: &mut impl Iterator<Item = GenericTokenTree>,
        name: String,
    ) -> Result<TokenTree> {
        // $ident

        let Some(GenericTokenTree::Terminal(Terminal::Colon)) = iter.next() else {
            return Err(());
        };

        // $ident:

        let Some(GenericTokenTree::Terminal(Terminal::Ident(kind))) = iter.next() else {
            return Err(());
        };
        let kind = kind.parse().map_err(drop)?;

        // $ident:kind

        Ok(TokenTree::Binding { name, kind })
    }

    fn parse_repetition(
        _iter: &mut impl Iterator<Item = GenericTokenTree>,
        _inner: Vec<GenericTokenTree>,
    ) -> Result<TokenTree> {
        todo!()
    }
}

#[derive(Clone, PartialEq)]
pub(crate) struct Matcher {
    pub(crate) bindings: HashMap<String, FragmentKind>,
}

impl std::fmt::Debug for Matcher {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // HashMaps are not ordered. As we need a deterministic debug output,
        // we have to make a shiny debug impl.

        struct Helper<'a>(Vec<(&'a String, &'a FragmentKind)>);

        impl std::fmt::Debug for Helper<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.debug_map().entries(self.0.iter().copied()).finish()
            }
        }

        let mut ordered_keys = self.bindings.iter().collect::<Vec<_>>();
        ordered_keys.sort_by_key(|&(name, _)| name);

        f.debug_struct("Matcher")
            .field("bindings", &Helper(ordered_keys))
            .finish()
    }
}

impl Matcher {
    pub(crate) fn from_generic(tokens: &[TokenTree]) -> Result<Matcher> {
        let mut bindings = HashMap::new();

        fn visit(bindings: &mut HashMap<String, FragmentKind>, token: &TokenTree) {
            match token {
                TokenTree::Terminal(_) => {}

                TokenTree::Binding { name, kind } => {
                    // TODO: properly check that there is no other binding with
                    // that name.
                    let prev = bindings.insert(name.clone(), *kind);
                    assert!(prev.is_none());
                }

                TokenTree::Repetition { inner, .. }
                | TokenTree::Parenthesed(inner)
                | TokenTree::CurlyBraced(inner) => inner.iter().for_each(|tt| visit(bindings, tt)),
            }
        }

        tokens.iter().for_each(|tt| visit(&mut bindings, tt));

        Ok(Matcher { bindings })
    }
}

macro_rules! matcher {
    // Nothing to parse
    (@inner [] [ $( $parsed:tt )* ]) => {
        vec![
            $( $parsed )*
        ]
    };

    // Parse an ident
    (@inner [$ident:ident $( $tail:tt )* ] [ $( $parsed:tt )* ]) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Terminal(
                    $crate::matcher::Terminal::Ident(stringify!($ident).to_string())
                ),
            ]
        }
    };

    // Parse parenthesed substream
    (@inner [( $( $inner:tt )* ) $( $tail:tt )*] [ $( $parsed:tt )* ]) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Parenthesed(
                    matcher! { $( $inner )* }
                ),
            ]
        }
    };

    // Parse +
    (@inner [+ $( $tail:tt )* ] [ $( $parsed:tt )* ]) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Terminal(
                    $crate::matcher::Terminal::Plus
                ),
            ]
        }
    };

    // Parse *
    (@inner [* $( $tail:tt )* ] [ $( $parsed:tt )* ]) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Terminal(
                    $crate::matcher::Terminal::Times
                ),
            ]
        }
    };

    // Parse binding
    (@inner [@ $name:ident : $kind:ident $( $tail:tt )* ] [ $( $parsed:tt )* ]) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Binding {
                    name: stringify!($name).to_string(),
                    kind: matcher!(@fragment_kind [$kind]),
                },
            ]
        }
    };

    // Parse repetition (1)
    (@inner [@( $( $inner:tt )* ) $( $tail:tt )* ] [ $( $parsed:tt )* ]) => {
        matcher! { @after_repetition
            [ $( $tail )* ]
            [ $( $parsed )* ]
            [matcher! { $( $inner )* }]
        }
    };

    // Parse repetition (2) - `?` matching
    (@after_repetition [? $( $tail:tt )* ] [ $( $parsed:tt )* ] [ $( $inner:tt )* ] $( [ $sep:expr ] )? ) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Repetition {
                    inner: $( $inner )*,
                    separator: array_to_opt([ $(
                        $sep
                    )*]),
                    quantifier: $crate::RepetitionQuantifier::ZeroOrOne,
                },
            ]
        }
    };

    // Parse repetition (3) - `*` matching
    (@after_repetition [* $( $tail:tt )* ] [ $( $parsed:tt )* ] [ $( $inner:tt )* ] $( [ $sep:expr ] )? ) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Repetition {
                    inner: $( $inner )*,
                    separator: array_to_opt([ $(
                        $sep
                    )*]),
                    quantifier: $crate::RepetitionQuantifier::ZeroOrMore,
                },
            ]
        }
    };


    // Parse repetition (4) - `+` matching
    (@after_repetition [+ $( $tail:tt )* ] [ $( $parsed:tt )* ] [ $( $inner:tt )* ] $( [ $sep:expr ] )? ) => {
        matcher! { @inner
            [ $( $tail )* ]
            [
                $( $parsed )*
                $crate::matcher::TokenTree::Repetition {
                    inner: $( $inner )*,
                    separator: array_to_opt([ $(
                        $sep
                    )*]),
                    quantifier: $crate::RepetitionQuantifier::OneOrMore,
                },
            ]
        }
    };

    // Parse repetition (5) - separator parsing
    (@after_repetition [ $sep:tt $( $tail:tt )* ] [ $( $parsed:tt )* ] [ $( $inner:tt )* ]) => {
        matcher! { @after_repetition
            [ $( $tail )* ]
            [ $( $parsed )* ]
            [ $( $inner )* ]
            [ Box::new(matcher! { $sep }.into_iter().next().unwrap()) ]
        }
    };

    // Parse ident fragment kind
    (@fragment_kind [ident]) => {
        crate::FragmentKind::Ident
    };

    // Parse expr fragment kind
    (@fragment_kind [expr]) => {
        crate::FragmentKind::Expr
    };

    // Entry point
    ( $( $tt:tt )* ) => {
        matcher! {
            @inner [ $( $tt )* ] []
        }
    }
}

// just a little helper
fn array_to_opt<const N: usize, T>(v: [T; N]) -> Option<T> {
    v.into_iter().next()
}

#[cfg(test)]
mod macros {
    use super::*;

    macro_rules! matcher_macro_test {
        ($test_name:ident {
            { $( $left:tt )* }, $right:expr $(,)?
        }) => {
            #[test]
            fn $test_name() {
                let rslt = matcher! {
                    $( $left )*
                };

                let expected = $right;

                expected.assert_debug_eq(&rslt);
            }
        };
    }

    matcher_macro_test! {
        just_idents {
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

    matcher_macro_test! {
        simple_algebra {
            { a + b * c },
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
                        Times,
                    ),
                    Terminal(
                        Ident(
                            "c",
                        ),
                    ),
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        parenthesis {
            { (a) b },
            expect_test::expect![[r#"
                [
                    Parenthesed(
                        [
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                    ),
                    Terminal(
                        Ident(
                            "b",
                        ),
                    ),
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        binding_1 {
            { @a: ident },
            expect_test::expect![[r#"
                [
                    Binding {
                        name: "a",
                        kind: Ident,
                    },
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        binding_2 {
            { @a: expr },
            expect_test::expect![[r#"
                [
                    Binding {
                        name: "a",
                        kind: Expr,
                    },
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        repetion_0_1 {
            { @( a )? },
            expect_test::expect![[r#"
                [
                    Repetition {
                        inner: [
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                        separator: None,
                        quantifier: ZeroOrOne,
                    },
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        repetition_0_n {
            { @( a )* },
            expect_test::expect![[r#"
                [
                    Repetition {
                        inner: [
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                        separator: None,
                        quantifier: ZeroOrMore,
                    },
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        repetition_1_n {
            { @( a )+ },
            expect_test::expect![[r#"
                [
                    Repetition {
                        inner: [
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                        separator: None,
                        quantifier: OneOrMore,
                    },
                ]
            "#]],
        }
    }

    matcher_macro_test! {
        repetition_with_sep {
            { @( a )b+ },
            expect_test::expect![[r#"
                [
                    Repetition {
                        inner: [
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                        separator: Some(
                            Terminal(
                                Ident(
                                    "b",
                                ),
                            ),
                        ),
                        quantifier: OneOrMore,
                    },
                ]
            "#]],
        }
    }
}

#[cfg(test)]
mod local_tree_to_matcher {
    use super::*;

    macro_rules! matcher_conv_test {
        ($test_name:ident {
            { $( $left:tt )* }, $right:expr $(,)?
        }) => {
            #[test]
            fn $test_name() {
                let tokens = matcher! {
                    $( $left )*
                };

                let rslt = Matcher::from_generic(&tokens);

                let expected = $right;

                expected.assert_debug_eq(&rslt);
            }
        };
    }

    matcher_conv_test! {
        without_bindings {
            { a b c + d },
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {},
                    },
                )
            "#]]
        }
    }

    matcher_conv_test! {
        multiple_bindings {
            { @a: ident + @b: expr },
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {
                            "a": Ident,
                            "b": Expr,
                        },
                    },
                )
            "#]],
        }
    }

    matcher_conv_test! {
        nesting {
            { (((@a: ident)))},
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {
                            "a": Ident,
                        },
                    },
                )
            "#]],
        }
    }

    matcher_conv_test! {
        handles_repetitions {
            { @( @a:ident )* },
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {
                            "a": Ident,
                        },
                    },
                )
            "#]],
        }
    }
}
