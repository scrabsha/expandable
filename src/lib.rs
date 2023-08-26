use std::str::FromStr;

use grammar::State;

use crate::matcher::Matcher;

#[cfg(test)]
#[macro_use]
mod macros;
mod expansion;
mod grammar;
mod matcher;
mod states;
mod substitution;

/// The whole point.
///
/// This functions takes all the tokens that have been passed to the macro
/// invocation and performs all the checks that have been implemented in this
/// crate.
pub fn check_macro(ctxt: InvocationContext, input: Vec<TokenTree>) -> Result<(), ()> {
    let mut iter = input.into_iter();

    while let Some(head) = iter.next() {
        let matcher = match head {
            TokenTree::Parenthesed(inner) => {
                let inner = matcher::TokenTree::from_generic(inner)?;
                Matcher::from_generic(&inner)?
            }
            _ => return Err(()),
        };

        match iter.next().ok_or(())? {
            TokenTree::Terminal(Terminal::FatArrow) => {}
            _ => return Err(()),
        }

        let substitution = match iter.next().ok_or(())? {
            TokenTree::CurlyBraced(inner) => substitution::TokenTree::from_generic(inner)?,
            _ => return Err(()),
        };

        expansion::check_arm(ctxt.to_state(), matcher, &substitution)?;

        if let Some(semi) = iter.next() {
            match semi {
                TokenTree::Terminal(Terminal::Semi) => {}
                _ => return Err(()),
            }
        }
    }

    Ok(())
}

/// An untyped tree of tokens.
///
/// This type allows the end-user to represent the tokens that is passed in the
/// macro invocation. It is not _exactly_ the same as [`proc_macro::TokenTree`],
/// as the tokens are grouped differently [^1]. Writing a
/// [`proc_macro::TokenTree`] -> [`TokenTree`] should not be too hard, but is
/// not the scope of this crate.
///
/// [^1]: For instance, `+=` is represented as a single token in declarative
/// macros but as "`+` followed by `=`" in procedural macros
/// ([ref][declarative-macro-tokens-and-procedural-macro-tokens]).
///
/// [`proc_macro::TokenTree`]:
///     https://doc.rust-lang.org/proc_macro/enum.TokenTree.html
/// [declarative-macro-tokens-and-procedural-macro-tokens]:
///     https://doc.rust-lang.org/reference/procedural-macros.html#declarative-macro-tokens-and-procedural-macro-tokens
#[derive(Clone, Debug, PartialEq)]
pub enum TokenTree {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree>),
    CurlyBraced(Vec<TokenTree>),
}

/// A terminal.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Terminal {
    Arrow,
    Colon,
    Dollar,
    FatArrow,
    Ident(String),
    Plus,
    QuestionMark,
    Semi,
    Times,
}

/// The contexts in which a macro can be called.
///
/// All macros can't be called in all contexts. For instance, a macro that
/// expands to a pattern may not be called where an expression is expected.
/// This type allows the [`check_macro`] function to know the context the macro
/// will be invoked in.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InvocationContext {
    /// The macro expands to an expression.
    Expr,
    /// The macro expands to any number of item.
    Item,
}

impl InvocationContext {
    fn to_state(&self) -> State {
        match self {
            InvocationContext::Expr => State::ExprStart,
            InvocationContext::Item => State::ItemStart,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum FragmentKind {
    Expr,
    Ident,
    Item,
}

impl FromStr for FragmentKind {
    type Err = ();

    fn from_str(s: &str) -> Result<FragmentKind, ()> {
        Ok(match s {
            "ident" => FragmentKind::Ident,
            "item" => FragmentKind::Item,
            "expr" => FragmentKind::Expr,

            _ => return Err(()),
        })
    }
}

impl FromStr for InvocationContext {
    type Err = ();

    fn from_str(s: &str) -> Result<InvocationContext, ()> {
        Ok(match s {
            "item" => InvocationContext::Item,
            "expr" => InvocationContext::Expr,

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
