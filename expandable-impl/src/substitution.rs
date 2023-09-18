// Architectural invariant: this module contains types and functions that allow
// to represent a macro substitution in a meaningful way.

use crate::{
    error::{Error, MacroRuleNode},
    RepetitionQuantifier, RepetitionQuantifierKind, Spannable, Terminal,
    TokenTree as GenericTokenTree, TokenTreeKind as GenericTokenTreeKind,
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

impl_spannable!(TokenTreeKind<Span> => TokenTree);

impl<Span> TokenTree<Span>
where
    Span: Copy,
{
    pub(crate) fn from_generic(
        value: Vec<GenericTokenTree<Span>>,
    ) -> Result<Vec<TokenTree<Span>>, Error<Span>> {
        let mut out = Vec::with_capacity(value.len());
        let mut iter = value.into_iter();

        while let Some(token) = iter.next() {
            let elem = match token.kind {
                GenericTokenTreeKind::Terminal(Terminal::Dollar) => {
                    let Some(token) = iter.next() else {
                        return Err(Error::UnexpectedEnd {
                            last_token: Some(token.span),
                        });
                    };

                    match token.kind {
                        GenericTokenTreeKind::Terminal(Terminal::Ident(ident)) => {
                            // TODO: expand the span?
                            TokenTreeKind::Fragment(ident).with_span(token.span)
                        }
                        GenericTokenTreeKind::Parenthesed(inner) => {
                            Self::parse_repetition(&mut iter, inner, token.span)?
                        }

                        GenericTokenTreeKind::CurlyBraced(_)
                        | GenericTokenTreeKind::Terminal(_) => {
                            return Err(Error::ParsingFailed {
                                what: vec![MacroRuleNode::Repetition, MacroRuleNode::FragmentName],
                                where_: token.span,
                            })
                        }
                    }
                }

                GenericTokenTreeKind::Terminal(t) => {
                    TokenTreeKind::Terminal(t).with_span(token.span)
                }

                GenericTokenTreeKind::Parenthesed(inner) => {
                    TokenTreeKind::Parenthesed(TokenTree::from_generic(inner)?)
                        .with_span(token.span)
                }

                GenericTokenTreeKind::CurlyBraced(inner) => {
                    TokenTreeKind::CurlyBraced(TokenTree::from_generic(inner)?)
                        .with_span(token.span)
                }
            };

            out.push(elem);
        }

        Ok(out)
    }

    fn parse_repetition(
        iter: &mut impl Iterator<Item = crate::TokenTree<Span>>,
        inner: Vec<GenericTokenTree<Span>>,
        start_span: Span,
    ) -> Result<TokenTree<Span>, Error<Span>> {
        let inner = Self::from_generic(inner)?;

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(start_span),
            });
        };

        let (separator, quantifier) = match token.kind {
            GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => {
                let quantifier = RepetitionQuantifierKind::ZeroOrOne.with_span(token.span);
                (None, quantifier)
            }

            GenericTokenTreeKind::Terminal(Terminal::Times) => {
                let quantifier = RepetitionQuantifierKind::ZeroOrMore.with_span(token.span);
                (None, quantifier)
            }

            GenericTokenTreeKind::Terminal(Terminal::Plus) => {
                let quantifier = RepetitionQuantifierKind::OneOrMore.with_span(token.span);
                (None, quantifier)
            }

            GenericTokenTreeKind::Terminal(t) => {
                let t = TokenTree {
                    kind: TokenTreeKind::Terminal(t),
                    span: token.span,
                };

                let Some(token) = iter.next() else {
                    return Err(Error::UnexpectedEnd {
                        last_token: Some(token.span),
                    });
                };

                let del = match token.kind {
                    GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => {
                        RepetitionQuantifierKind::ZeroOrOne.with_span(token.span)
                    }

                    GenericTokenTreeKind::Terminal(Terminal::Times) => {
                        RepetitionQuantifierKind::ZeroOrMore.with_span(token.span)
                    }

                    GenericTokenTreeKind::Terminal(Terminal::Plus) => {
                        RepetitionQuantifierKind::OneOrMore.with_span(token.span)
                    }

                    GenericTokenTreeKind::Terminal(_)
                    | GenericTokenTreeKind::Parenthesed(_)
                    | GenericTokenTreeKind::CurlyBraced(_) => {
                        return Err(Error::ParsingFailed {
                            what: vec![MacroRuleNode::RepetitionQuantifier],
                            where_: token.span,
                        })
                    }
                };

                (Some(t), del)
            }
            GenericTokenTreeKind::Parenthesed(_) | GenericTokenTreeKind::CurlyBraced(_) => {
                return Err(Error::ParsingFailed {
                    what: vec![MacroRuleNode::RepetitionQuantifier],
                    where_: token.span,
                })
            }
        };

        let separator = separator.map(Box::new);

        Ok(TokenTreeKind::Repetition {
            inner,
            separator,
            quantifier,
        }
        // TODO
        .with_span(token.span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! conversion_test {
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

    conversion_test! {
        just_idents {
            { a b c },
            expect_test::expect![[r#"
                Ok(
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
                    ],
                )
            "#]]
        }
    }

    conversion_test! {
        with_fragment {
            { @a },
            expect_test::expect![[r#"
                Ok(
                    [
                        TokenTree {
                            kind: Fragment(
                                "a",
                            ),
                            span: (),
                        },
                    ],
                )
            "#]],
        }
    }

    conversion_test! {
        with_repetition {
            { @( test )* },
            expect_test::expect![[r#"
                Ok(
                    [
                        TokenTree {
                            kind: Repetition {
                                inner: [
                                    TokenTree {
                                        kind: Terminal(
                                            Ident(
                                                "test",
                                            ),
                                        ),
                                        span: (),
                                    },
                                ],
                                separator: None,
                                quantifier: RepetitionQuantifier {
                                    kind: ZeroOrMore,
                                    span: (),
                                },
                            },
                            span: (),
                        },
                    ],
                )
            "#]],
        }
    }

    conversion_test! {
        with_repetition_and_fragment {
            { @( @a )? },
            expect_test::expect![[r#"
                Ok(
                    [
                        TokenTree {
                            kind: Repetition {
                                inner: [
                                    TokenTree {
                                        kind: Fragment(
                                            "a",
                                        ),
                                        span: (),
                                    },
                                ],
                                separator: None,
                                quantifier: RepetitionQuantifier {
                                    kind: ZeroOrOne,
                                    span: (),
                                },
                            },
                            span: (),
                        },
                    ],
                )
            "#]],
        }
    }
}
