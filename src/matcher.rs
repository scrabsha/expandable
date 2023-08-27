use std::collections::HashMap;

use crate::{
    FragmentKind, RepetitionQuantifier, RepetitionQuantifierKind, Terminal,
    TokenTree as GenericTokenTree, TokenTreeKind as GenericTokenTreeKind,
};

type Result<T> = std::result::Result<T, ()>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TokenTree<Span> {
    kind: TokenTreeKind<Span>,
    span: Span,
}

#[cfg(test)]
#[allow(non_snake_case)]
impl TokenTree<()> {
    fn Terminal(t: Terminal) -> TokenTree<()> {
        TokenTree {
            kind: TokenTreeKind::Terminal(t),
            span: (),
        }
    }

    fn Parenthesed(i: Vec<TokenTree<()>>) -> TokenTree<()> {
        TokenTree {
            kind: TokenTreeKind::Parenthesed(i),
            span: (),
        }
    }

    fn CurlyBraced(i: Vec<TokenTree<()>>) -> TokenTree<()> {
        TokenTree {
            kind: TokenTreeKind::CurlyBraced(i),
            span: (),
        }
    }

    fn Binding(name: String, kind: FragmentKind) -> TokenTree<()> {
        TokenTree {
            kind: TokenTreeKind::Binding { name, kind },
            span: (),
        }
    }

    fn Repetition(
        inner: Vec<TokenTree<()>>,
        separator: Option<TokenTree<()>>,
        quantifier: RepetitionQuantifier<()>,
    ) -> TokenTree<()> {
        let separator = separator.map(Box::new);
        TokenTree {
            kind: TokenTreeKind::Repetition {
                inner,
                separator,
                quantifier,
            },
            span: (),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TokenTreeKind<Span> {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree<Span>>),
    CurlyBraced(Vec<TokenTree<Span>>),
    Binding {
        name: String,
        kind: FragmentKind,
    },
    Repetition {
        inner: Vec<TokenTree<Span>>,
        separator: Option<Box<TokenTree<Span>>>,
        quantifier: RepetitionQuantifier<Span>,
    },
}

impl<Span> TokenTree<Span> {
    pub(crate) fn from_generic(
        generic: Vec<GenericTokenTree<Span>>,
    ) -> Result<Vec<TokenTree<Span>>> {
        let mut out = Vec::with_capacity(generic.len());
        let mut iter = generic.into_iter();

        while let Some(token) = iter.next() {
            let token = match token.kind {
                // $
                GenericTokenTreeKind::Terminal(Terminal::Dollar) => {
                    let after_dollar = iter.next().ok_or(())?;
                    match after_dollar.kind {
                        // $ident
                        GenericTokenTreeKind::Terminal(Terminal::Ident(id)) => {
                            Self::parse_fragment(&mut iter, id, after_dollar.span)
                        }
                        // $(...)
                        GenericTokenTreeKind::Parenthesed(inner) => {
                            Self::parse_repetition(&mut iter, inner, after_dollar.span)
                        }

                        GenericTokenTreeKind::Terminal(_)
                        | GenericTokenTreeKind::CurlyBraced(_) => return Err(()),
                    }?
                }

                GenericTokenTreeKind::Terminal(t) => TokenTree {
                    kind: TokenTreeKind::Terminal(t),
                    span: token.span,
                },

                GenericTokenTreeKind::Parenthesed(inner) => TokenTree {
                    kind: TokenTreeKind::Parenthesed(TokenTree::from_generic(inner)?),
                    span: token.span,
                },

                GenericTokenTreeKind::CurlyBraced(inner) => TokenTree {
                    kind: TokenTreeKind::CurlyBraced(TokenTree::from_generic(inner)?),
                    span: token.span,
                },
            };

            out.push(token);
        }

        Ok(out)
    }

    fn parse_fragment(
        iter: &mut impl Iterator<Item = GenericTokenTree<Span>>,
        name: String,
        span: Span,
    ) -> Result<TokenTree<Span>> {
        // $ident

        let token = iter.next().ok_or(())?;
        let GenericTokenTreeKind::Terminal(Terminal::Colon) = token.kind else {
            return Err(());
        };
        // TODO: we want to be able to expand the span somehow.
        let span = token.span;

        // $ident:

        let token = iter.next().ok_or(())?;
        let GenericTokenTreeKind::Terminal(Terminal::Ident(kind)) = token.kind else {
            return Err(());
        };
        let kind = kind.parse().map_err(drop)?;

        // $ident:kind

        Ok(TokenTree {
            kind: TokenTreeKind::Binding { name, kind },
            span,
        })
    }

    fn parse_repetition(
        iter: &mut impl Iterator<Item = GenericTokenTree<Span>>,
        inner: Vec<GenericTokenTree<Span>>,
        span: Span,
    ) -> Result<TokenTree<Span>> {
        let inner = TokenTree::from_generic(inner)?;

        let token = iter.next().ok_or(())?;
        let (separator, quantifier) = match token.kind {
            GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => (
                None,
                RepetitionQuantifier {
                    kind: RepetitionQuantifierKind::ZeroOrOne,
                    span: token.span,
                },
            ),
            GenericTokenTreeKind::Terminal(Terminal::Times) => (
                None,
                RepetitionQuantifier {
                    kind: RepetitionQuantifierKind::ZeroOrMore,
                    span: token.span,
                },
            ),
            GenericTokenTreeKind::Terminal(Terminal::Plus) => (
                None,
                RepetitionQuantifier {
                    kind: RepetitionQuantifierKind::OneOrMore,
                    span: token.span,
                },
            ),
            GenericTokenTreeKind::Terminal(sep) => {
                let sep = TokenTree {
                    kind: TokenTreeKind::Terminal(sep),
                    span: token.span,
                };

                let token = iter.next().ok_or(())?;
                let kind = match token.kind {
                    GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => {
                        RepetitionQuantifierKind::ZeroOrOne
                    }
                    GenericTokenTreeKind::Terminal(Terminal::Times) => {
                        RepetitionQuantifierKind::ZeroOrMore
                    }
                    GenericTokenTreeKind::Terminal(Terminal::Plus) => {
                        RepetitionQuantifierKind::OneOrMore
                    }

                    _ => return Err(()),
                };
                let quantifier = RepetitionQuantifier {
                    kind,
                    span: token.span,
                };

                (Some(Box::new(sep)), quantifier)
            }
            GenericTokenTreeKind::Parenthesed(_) | GenericTokenTreeKind::CurlyBraced(_) => {
                return Err(())
            }
        };

        Ok(TokenTree {
            kind: TokenTreeKind::Repetition {
                inner,
                separator,
                quantifier,
            },
            span,
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
    pub(crate) fn from_generic<Span>(tokens: &[TokenTree<Span>]) -> Result<Matcher> {
        let mut bindings = HashMap::new();

        fn visit<Span>(bindings: &mut HashMap<String, FragmentKind>, token: &TokenTree<Span>) {
            match &token.kind {
                TokenTreeKind::Terminal(_) => {}

                TokenTreeKind::Binding { name, kind } => {
                    // TODO: properly check that there is no other binding with
                    // that name.
                    let prev = bindings.insert(name.clone(), *kind);
                    assert!(prev.is_none());
                }

                TokenTreeKind::Repetition { inner, .. }
                | TokenTreeKind::Parenthesed(inner)
                | TokenTreeKind::CurlyBraced(inner) => {
                    inner.iter().for_each(|tt| visit(bindings, tt))
                }
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
