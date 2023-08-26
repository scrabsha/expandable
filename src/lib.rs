use std::str::FromStr;

use grammar::{DynamicState, State};

#[macro_use]
mod macros;
mod expansion;
mod grammar;
mod matcher;
mod states;
mod substitution;

/// An untyped tree of tokens.
///
/// This is converted into various, more context-specific versions of this
/// type are defined in the other submodules:
/// - [`matcher::TokenTree`]
/// - [`substitution::TokenTree`],
#[derive(Clone, Debug, PartialEq)]
enum TokenTree {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree>),
    CurlyBraced(Vec<TokenTree>),
}

/// A terminal.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Terminal {
    Colon,
    Dollar,
    Ident(String),
    Plus,
    QuestionMark,
    Times,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum FragmentKind {
    Item,
    Ident,
    Expr,
}

impl FragmentKind {
    pub(crate) fn to_dynamic_state(self) -> DynamicState {
        match self {
            FragmentKind::Item => State::ItemStart.into_dynamic_state(),
            FragmentKind::Ident => State::ExprStart.into_dynamic_state(),
            FragmentKind::Expr => State::ExprStart.into_dynamic_state(),
        }
    }
}

impl FromStr for FragmentKind {
    type Err = ();

    fn from_str(s: &str) -> Result<FragmentKind, ()> {
        Ok(match s {
            "item" => FragmentKind::Item,
            "ident" => FragmentKind::Ident,
            "expr" => FragmentKind::Expr,

            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum RepetitionQuantifier {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}

#[macro_export]
#[cfg(test)]
mod macros_ {
    macro_rules! token_tree_test {
        ($name:ident {
            { $( $left:tt )* },
            $right:expr $(,)?
        }) => {
            #[test]
            fn $name() {
                let left = token_tree! { $( $left )* };
                let right = $right;

                right.assert_debug_eq(&left);
            }
        }
    }

    token_tree_test! {
        simple_traduction {
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

    token_tree_test! {
        handles_matcher_syntax {
            { a + b @( @test:ident )+* },
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
                        Dollar,
                    ),
                    Parenthesed(
                        [
                            Terminal(
                                Dollar,
                            ),
                            Terminal(
                                Ident(
                                    "test",
                                ),
                            ),
                            Terminal(
                                Colon,
                            ),
                            Terminal(
                                Ident(
                                    "ident",
                                ),
                            ),
                        ],
                    ),
                    Terminal(
                        Plus,
                    ),
                    Terminal(
                        Times,
                    ),
                ]
            "#]],
        }
    }

    token_tree_test! {
        can_parse_substition_syntax {
            { a + b @( + @c + a )+? },
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
                        Dollar,
                    ),
                    Parenthesed(
                        [
                            Terminal(
                                Plus,
                            ),
                            Terminal(
                                Dollar,
                            ),
                            Terminal(
                                Ident(
                                    "c",
                                ),
                            ),
                            Terminal(
                                Plus,
                            ),
                            Terminal(
                                Ident(
                                    "a",
                                ),
                            ),
                        ],
                    ),
                    Terminal(
                        Plus,
                    ),
                    Terminal(
                        QuestionMark,
                    ),
                ]
            "#]],
        }
    }
}
