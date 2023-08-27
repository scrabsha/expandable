use crate::{
    RepetitionQuantifier, RepetitionQuantifierKind, Spanneable, Terminal,
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

impl<Span> Spanneable<Span> for TokenTreeKind<Span> {
    type Output = TokenTree<Span>;

    fn with_span(self, span: Span) -> TokenTree<Span> {
        TokenTree { kind: self, span }
    }
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
                            TokenTreeKind::Fragment(ident).with_span(token.span)
                        }
                        GenericTokenTreeKind::Parenthesed(inner) => {
                            Self::parse_repetition(&mut iter, inner)?
                        }

                        GenericTokenTreeKind::CurlyBraced(_)
                        | GenericTokenTreeKind::Terminal(_) => return Err(()),
                    }
                }

                GenericTokenTreeKind::Terminal(t) => {
                    TokenTreeKind::Terminal(t).with_span(tree.span)
                }

                GenericTokenTreeKind::Parenthesed(inner) => {
                    TokenTreeKind::Parenthesed(TokenTree::from_generic(inner)?).with_span(tree.span)
                }

                GenericTokenTreeKind::CurlyBraced(inner) => {
                    TokenTreeKind::CurlyBraced(TokenTree::from_generic(inner)?).with_span(tree.span)
                }
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

                let token = iter.next().ok_or(())?;
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
                    | GenericTokenTreeKind::CurlyBraced(_) => return Err(()),
                };

                (Some(t), del)
            }
            GenericTokenTreeKind::Parenthesed(_) | GenericTokenTreeKind::CurlyBraced(_) => {
                return Err(())
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
