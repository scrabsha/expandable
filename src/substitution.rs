use crate::{
    RepetitionQuantifier, RepetitionQuantifierKind, Terminal, TokenTree as GenericTokenTree,
    TokenTreeKind as GenericTokenTreeKind,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct TokenTree<Span> {
    pub(crate) kind: TokenTreeKind<Span>,
    pub(crate) span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum TokenTreeKind<Span> {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree<Span>>),
    CurlyBraced(Vec<TokenTree<Span>>),
    Fragment(String),
    Repetition {
        inner: Vec<TokenTree<Span>>,
        separator: Option<Box<TokenTree<Span>>>,
        quantifier: RepetitionQuantifier<Span>,
    },
}

impl<Span> TokenTree<Span>
where
    Span: Copy,
{
    pub(crate) fn from_generic(
        value: Vec<GenericTokenTree<Span>>,
    ) -> Result<Vec<TokenTree<Span>>, ()> {
        let mut out = Vec::with_capacity(value.len());
        let mut iter = value.into_iter();

        while let Some(tree) = iter.next() {
            let elem = match tree.kind {
                GenericTokenTreeKind::Terminal(Terminal::Dollar) => {
                    let token = iter.next().ok_or(())?;
                    match token.kind {
                        GenericTokenTreeKind::Terminal(Terminal::Ident(ident)) => {
                            // TODO: expand the span?
                            TokenTree {
                                kind: TokenTreeKind::Fragment(ident),
                                span: token.span,
                            }
                        }
                        GenericTokenTreeKind::Parenthesed(inner) => {
                            Self::parse_repetition(&mut iter, inner)?
                        }

                        GenericTokenTreeKind::CurlyBraced(_)
                        | GenericTokenTreeKind::Terminal(_) => return Err(()),
                    }
                }

                GenericTokenTreeKind::Terminal(t) => TokenTree {
                    kind: TokenTreeKind::Terminal(t),
                    span: tree.span,
                },
                GenericTokenTreeKind::Parenthesed(inner) => TokenTree {
                    kind: TokenTreeKind::Parenthesed(TokenTree::from_generic(inner)?),
                    span: tree.span,
                },
                GenericTokenTreeKind::CurlyBraced(inner) => TokenTree {
                    kind: TokenTreeKind::CurlyBraced(TokenTree::from_generic(inner)?),
                    span: tree.span,
                },
            };

            out.push(elem);
        }

        Ok(out)
    }

    fn parse_repetition(
        iter: &mut impl Iterator<Item = crate::TokenTree<Span>>,
        inner: Vec<GenericTokenTree<Span>>,
    ) -> Result<TokenTree<Span>, ()> {
        let inner = Self::from_generic(inner)?;

        let token = iter.next().ok_or(())?;
        let (separator, quantifier) = match token.kind {
            GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => {
                let quantifier = RepetitionQuantifier {
                    kind: RepetitionQuantifierKind::ZeroOrOne,
                    span: token.span,
                };

                (None, quantifier)
            }

            GenericTokenTreeKind::Terminal(Terminal::Times) => {
                let quantifier = RepetitionQuantifier {
                    kind: RepetitionQuantifierKind::ZeroOrMore,
                    span: token.span,
                };

                (None, quantifier)
            }

            GenericTokenTreeKind::Terminal(Terminal::Plus) => {
                let quantifier = RepetitionQuantifier {
                    kind: RepetitionQuantifierKind::OneOrMore,
                    span: token.span,
                };

                (None, quantifier)
            }

            GenericTokenTreeKind::Terminal(t) => {
                let t = TokenTree {
                    kind: TokenTreeKind::Terminal(t),
                    span: token.span,
                };

                let token = iter.next().ok_or(())?;
                let del = match token.kind {
                    GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => {
                        RepetitionQuantifier {
                            kind: RepetitionQuantifierKind::ZeroOrOne,
                            span: token.span,
                        }
                    }
                    GenericTokenTreeKind::Terminal(Terminal::Times) => RepetitionQuantifier {
                        kind: RepetitionQuantifierKind::ZeroOrMore,
                        span: token.span,
                    },
                    GenericTokenTreeKind::Terminal(Terminal::Plus) => RepetitionQuantifier {
                        kind: RepetitionQuantifierKind::OneOrMore,
                        span: token.span,
                    },

                    GenericTokenTreeKind::Terminal(_)
                    | GenericTokenTreeKind::Parenthesed(_)
                    | GenericTokenTreeKind::CurlyBraced(_) => return Err(()),
                };

                (Some(t), del)
            }
            GenericTokenTreeKind::Parenthesed(_) | GenericTokenTreeKind::CurlyBraced(_) => {
                return Err(())
            }
        };

        let separator = separator.map(Box::new);

        Ok(TokenTree {
            kind: TokenTreeKind::Repetition {
                inner,
                separator,
                quantifier,
            },
            // TODO
            span: token.span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! convertion_test {
        ( $name:ident {
            { $( $left:tt )* },
            $right:expr $(,)?
        }) => {
            #[test]
            fn $name() {
                let left = TokenTree::from_generic(quote! { $( $left )* });
                let right = $right;

                right.assert_debug_eq(&left);
            }
        };
    }

    convertion_test! {
        just_idents {
            { a b c },
            expect_test::expect![[r#"
                Ok(
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
                    ],
                )
            "#]]
        }
    }

    convertion_test! {
        with_fragment {
            { @a },
            expect_test::expect![[r#"
                Ok(
                    [
                        Fragment(
                            "a",
                        ),
                    ],
                )
            "#]],
        }
    }

    convertion_test! {
        with_repetition {
            { @( test )* },
            expect_test::expect![[r#"
                Ok(
                    [
                        Repetition {
                            inner: [
                                Terminal(
                                    Ident(
                                        "test",
                                    ),
                                ),
                            ],
                            separator: None,
                            quantifier: ZeroOrMore,
                        },
                    ],
                )
            "#]],
        }
    }

    convertion_test! {
        with_repetition_and_fragment {
            { @( @a )? },
            expect_test::expect![[r#"
                Ok(
                    [
                        Repetition {
                            inner: [
                                Fragment(
                                    "a",
                                ),
                            ],
                            separator: None,
                            quantifier: ZeroOrOne,
                        },
                    ],
                )
            "#]],
        }
    }
}
