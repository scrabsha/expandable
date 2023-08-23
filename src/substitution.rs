use crate::{RepetitionQuantifier, Terminal, TokenTree as GenericTokenTree};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum TokenTree {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree>),
    Fragment(String),
    Repetition {
        inner: Vec<TokenTree>,
        separator: Option<Box<TokenTree>>,
        quantifier: RepetitionQuantifier,
    },
}

impl TokenTree {
    pub(crate) fn from_generic(value: Vec<GenericTokenTree>) -> Result<Vec<TokenTree>, ()> {
        let mut out = Vec::with_capacity(value.len());
        let mut iter = value.into_iter();

        while let Some(tree) = iter.next() {
            let elem = match tree {
                GenericTokenTree::Terminal(Terminal::Dollar) => match iter.next().ok_or(())? {
                    GenericTokenTree::Terminal(Terminal::Ident(ident)) => {
                        TokenTree::Fragment(ident)
                    }
                    GenericTokenTree::Parenthesed(inner) => {
                        Self::parse_repetition(&mut iter, inner)?
                    }

                    GenericTokenTree::Terminal(_) => return Err(()),
                },

                GenericTokenTree::Terminal(t) => TokenTree::Terminal(t),
                GenericTokenTree::Parenthesed(i) => {
                    TokenTree::Parenthesed(TokenTree::from_generic(i)?)
                }
            };

            out.push(elem);
        }

        Ok(out)
    }

    fn parse_repetition(
        iter: &mut impl Iterator<Item = crate::TokenTree>,
        inner: Vec<crate::TokenTree>,
    ) -> Result<TokenTree, ()> {
        let inner = Self::from_generic(inner)?;

        let (separator, quantifier) = match iter.next().ok_or(())? {
            GenericTokenTree::Terminal(Terminal::QuestionMark) => {
                (None, RepetitionQuantifier::ZeroOrOne)
            }

            GenericTokenTree::Terminal(Terminal::Times) => (None, RepetitionQuantifier::ZeroOrMore),

            GenericTokenTree::Terminal(Terminal::Plus) => (None, RepetitionQuantifier::OneOrMore),

            GenericTokenTree::Terminal(t) => {
                let t = TokenTree::Terminal(t);
                let del = match iter.next().ok_or(())? {
                    GenericTokenTree::Terminal(Terminal::QuestionMark) => {
                        RepetitionQuantifier::ZeroOrOne
                    }
                    GenericTokenTree::Terminal(Terminal::Times) => RepetitionQuantifier::ZeroOrMore,
                    GenericTokenTree::Terminal(Terminal::Plus) => RepetitionQuantifier::OneOrMore,

                    GenericTokenTree::Terminal(_) | GenericTokenTree::Parenthesed(_) => {
                        return Err(())
                    }
                };

                (Some(t), del)
            }
            crate::TokenTree::Parenthesed(_) => return Err(()),
        };

        let separator = separator.map(Box::new);

        Ok(TokenTree::Repetition {
            inner,
            separator,
            quantifier,
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
                let left = TokenTree::from_generic(token_tree! { $( $left )* });
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
