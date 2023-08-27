use std::{marker::Copy, str::FromStr};

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
pub fn check_macro<Span>(ctxt: InvocationContext, input: Vec<TokenTree<Span>>) -> Result<(), ()>
where
    Span: Copy,
{
    let mut iter = input.into_iter();

    while let Some(head) = iter.next() {
        let matcher = match head.kind {
            TokenTreeKind::Parenthesed(inner) => {
                let inner = matcher::TokenTree::from_generic(inner)?;
                Matcher::from_generic(&inner)?
            }
            _ => return Err(()),
        };

        match iter.next().ok_or(())?.kind {
            TokenTreeKind::Terminal(Terminal::FatArrow) => {}
            _ => return Err(()),
        }

        let substitution = match iter.next().ok_or(())?.kind {
            TokenTreeKind::CurlyBraced(inner) => substitution::TokenTree::from_generic(inner)?,
            _ => return Err(()),
        };

        expansion::check_arm(ctxt.to_state(), matcher, &substitution)?;

        if let Some(semi) = iter.next() {
            match semi.kind {
                TokenTreeKind::Terminal(Terminal::Semi) => {}
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
pub struct TokenTree<Span> {
    pub kind: TokenTreeKind<Span>,
    pub span: Span,
}

pub trait Spanneable<Span> {
    type Output;

    fn with_span(self, span: Span) -> Self::Output;
}

#[cfg(test)]
#[allow(non_snake_case, unused)]
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenTreeKind<Span> {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree<Span>>),
    CurlyBraced(Vec<TokenTree<Span>>),
}

impl<Span> Spanneable<Span> for TokenTreeKind<Span> {
    type Output = TokenTree<Span>;

    fn with_span(self, span: Span) -> TokenTree<Span> {
        TokenTree { kind: self, span }
    }
}

/// A terminal symbol.
///
/// # Multicharacter operators
///
/// Multicharacter operators (`+=`, `->`, ...) must _not_ be split in multiple
/// [`Terminal`]. Any use of the [`check_macro`] function that does not respect
/// this invariant will is subject to unexpected results.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Terminal {
    /// An arrow (`->`).
    Arrow,
    /// A colon (':').
    Colon,
    /// A dollar (`@`).
    Dollar,
    /// A fat arrow (`=>`).
    FatArrow,
    /// An identifier (`foo`, `bar`).
    Ident(String),
    /// A plus (`+`).
    Plus,
    /// A question mark (`?`).
    QuestionMark,
    /// A semicolon (`;`).
    Semi,
    /// A times (`*`).
    Times,
}

impl<Span> Spanneable<Span> for Terminal {
    type Output = TokenTree<Span>;

    fn with_span(self, span: Span) -> TokenTree<Span> {
        TokenTree {
            kind: self.into(),
            span,
        }
    }
}

impl<Span> From<Terminal> for TokenTreeKind<Span> {
    fn from(value: Terminal) -> TokenTreeKind<Span> {
        TokenTreeKind::Terminal(value)
    }
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
struct RepetitionQuantifier<Span> {
    kind: RepetitionQuantifierKind,
    span: Span,
}

#[cfg(test)]
#[allow(non_snake_case, unused)]
impl RepetitionQuantifier<()> {
    fn ZeroOrOne() -> RepetitionQuantifier<()> {
        RepetitionQuantifier {
            kind: RepetitionQuantifierKind::ZeroOrOne,
            span: (),
        }
    }

    fn ZeroOrMore() -> RepetitionQuantifier<()> {
        RepetitionQuantifier {
            kind: RepetitionQuantifierKind::ZeroOrMore,
            span: (),
        }
    }

    fn OneOrMore() -> RepetitionQuantifier<()> {
        RepetitionQuantifier {
            kind: RepetitionQuantifierKind::OneOrMore,
            span: (),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum RepetitionQuantifierKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}

impl<Span> Spanneable<Span> for RepetitionQuantifierKind {
    type Output = RepetitionQuantifier<Span>;

    fn with_span(self, span: Span) -> RepetitionQuantifier<Span> {
        RepetitionQuantifier { kind: self, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! check_macro_test {
        ( $test_name:ident {
            #[$kind:ident]
            {
                $( $tt:tt )*
            }
        }) => {
            #[test]
            fn $test_name() {
                let tokens = quote! { $( $tt )* };
                let ctxt = stringify!($kind).parse::<InvocationContext>().expect("Failed to parse `InvocationContext`");

                check_macro(ctxt, tokens).unwrap();
            }
        };
    }

    check_macro_test! {
        single_arm {
            #[expr]
            {
                () => { a }
            }
        }
    }

    check_macro_test! {
        accepts_final_semi {
            #[expr]
            {
                () => { a };
            }
        }
    }

    check_macro_test! {
        multiple_arms {
            #[expr]
            {
                () => { a };
                (()) => { b };
            }
        }
    }
}
