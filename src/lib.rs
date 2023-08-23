use std::str::FromStr;

macro_rules! token_tree {
    (@inner, ( $( $tt:tt )* ) ) => {
        $crate::TokenTree::Parenthesed(token_tree! { $( $tt )* })
    };

    (@inner, $id:ident) => {
        $crate::TokenTree::Terminal($crate::Terminal::Ident(stringify!($id).to_string()))
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

    ( $( $tt:tt )* ) => {
        vec![
            $(
                token_tree!(@inner, $tt)
            ),*
        ]
    };
}

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
    Ident,
    Expr,
}

impl FromStr for FragmentKind {
    type Err = ();

    fn from_str(s: &str) -> Result<FragmentKind, ()> {
        Ok(match s {
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
mod macros {
    use super::*;

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
