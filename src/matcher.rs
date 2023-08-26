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
        iter: &mut impl Iterator<Item = GenericTokenTree>,
        inner: Vec<GenericTokenTree>,
    ) -> Result<TokenTree> {
        let inner = TokenTree::from_generic(inner)?;

        let (separator, quantifier) = match iter.next().ok_or(())? {
            GenericTokenTree::Terminal(Terminal::QuestionMark) => {
                (None, RepetitionQuantifier::ZeroOrOne)
            }
            GenericTokenTree::Terminal(Terminal::Times) => (None, RepetitionQuantifier::ZeroOrMore),
            GenericTokenTree::Terminal(Terminal::Plus) => (None, RepetitionQuantifier::OneOrMore),
            GenericTokenTree::Terminal(sep) => {
                let quantifier = match iter.next().ok_or(())? {
                    GenericTokenTree::Terminal(Terminal::QuestionMark) => {
                        RepetitionQuantifier::ZeroOrOne
                    }
                    GenericTokenTree::Terminal(Terminal::Times) => RepetitionQuantifier::ZeroOrMore,
                    GenericTokenTree::Terminal(Terminal::Plus) => RepetitionQuantifier::OneOrMore,

                    _ => return Err(()),
                };

                (Some(Box::new(TokenTree::Terminal(sep))), quantifier)
            }
            GenericTokenTree::Parenthesed(_) | GenericTokenTree::CurlyBraced(_) => return Err(()),
        };

        Ok(TokenTree::Repetition {
            inner,
            separator,
            quantifier,
        })
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

#[cfg(test)]
mod local_tree_to_matcher {
    use super::*;

    macro_rules! matcher_conv_test {
        ($test_name:ident {
            { $( $left:tt )* }, $right:expr $(,)?
        }) => {
            #[test]
            fn $test_name() {
                let tokens = quote! {
                    $( $left )*
                };

                let tokens = TokenTree::from_generic(tokens).expect("Failed to convert to `matcher::TokenTree`");
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
