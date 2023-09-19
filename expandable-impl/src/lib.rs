#![deny(missing_debug_implementations)]
#![warn(
    missing_docs,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss,
    clippy::cast_lossless,
    clippy::cast_possible_wrap,
    clippy::clear_with_drain,
    clippy::dbg_macro,
    clippy::deref_by_slicing,
    clippy::doc_link_with_quotes,
    clippy::doc_markdown,
    clippy::explicit_deref_methods,
    clippy::get_unwrap,
    clippy::impl_trait_in_params,
    clippy::inefficient_to_string,
    clippy::redundant_else,
    clippy::semicolon_if_nothing_returned,
    clippy::should_panic_without_expect,
    clippy::string_add,
    clippy::string_to_string,
    clippy::used_underscore_binding,
    clippy::wildcard_imports
)]

//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable-impl</code></h1>
//! A runtime-agnostic <code>macro_rules!</code> expansion checker.
//! </div>
//!
//! ## Why?
//!
//! Let's consider the following Rust code:
//!
//! ```rust
//! macro_rules! js_concat {
//!     ($left:expr, $right:expr) => {
//!         $left ++ $right
//!     };
//! }
//! ```
//!
//! This macro is obviously not correct, as the `++` operator does not exist in
//! Rust. Any call to the `js_concat` macro will result in a compilation error.
//! However, the snippet above compiles.
//!
//! Let's now consider the following equally incorrect Rust code:
//!
//! ```rust,compile_fail,E0277
//! fn add(a: u8, b: char) {
//!     a + b;
//! }
//! ```
//!
//! This code is incorrect because `char` cannot be added to `u8`
//! [^error-message]. Rustc rightfully refuses to compile this snippet and emits
//! a cute error message we all love and cherish.
//!
//! Interestingly, macros and functions share a lot of things in common:
//! - They take _things_ an input and return _things_ as well,
//! - We have some information about the kind of _things_ that they take is
//! input[^things].
//!
//! So that's a bit unfair: why would functions have so much checks when macros
//! don't?
//!
//! <video controls >
//!     <!-- Yes, this is me using github as a CDN -->
//!     <source src="https://github.com/scrabsha/expendable/raw/main/assets/objection.mp4" type="video/mp4" />
//! </video>
//!
//! That's the purpose of this crate.
//!
//! ## What?
//!
//! This crate provides a _reasonably simple_[^simple] algorithm aiming to
//! guarantee that any call to a specific macro that match one of its rule will
//! produce a parseable output. Beside the macro definition itself, the only
//! required additional information is the context in which the macro will be
//! called[^weakness].
//!
//! It also handles poorly recursive macros. For now, it treats any macro
//! invocation occurring in macro expansion as an obscure chunk of code. It
//! does not check that this macro invocation will match any of its rule and
//! does not try to guess if its expansion is valid in the context it is called.
//! This restriction may be lifted in the future.
//!
//! ## Case study
//!
//!
//!
//! ## Usage
//!
//! The entry point of this crate is the [`check_macro`] function. The library
//! user gives to this function the content of the macro to be checked (as a
//! sequence of [`TokenTree`], as well as the context the macro should be called
//! in (as an [`InvocationContext`]). The crate machinery will then check the
//! macro content, and return any error it encounters.
//!
//! ## Spanning
//!
//! In order to stay as reusable as possible, all the data structures
//! representing AST nodes have a generic `Span` parameter. This allows library
//! users to use the span type provided by their use case without any trouble.
//! The only requirement is that the `Span` must be `Copy`. There may be more
//! restrictions in the future.
//!
//! [^error-message]: I'm just paraphrasing the `rustc` output here. Nothing too
//!     controversial here.
//!
//! [^things]: Rust functions also specify information about the _things_ that they
//!     return. That's actually the only weakness of this crate 😭.
//!
//! [^simple]: It is definitely simpler than most industrial static analyzer.
//!
//! [^weakness]: That is, whether if the macro will be called in an expression context,
//!     a pattern context, or an item context. This restriction may be slightly
//!     lifted in the future: each macro arm should be able to override this
//!     context.

pub use error::{Error, MacroRuleNode};

pub use grammar::TokenDescription;

use std::{marker::Copy, str::FromStr};

use grammar::State;

#[macro_use]
mod macros;
mod error;
mod expansion;
mod grammar;
mod list;
mod matcher;
mod repetition_stack;
mod states;
mod substitution;

/// The whole point.
///
/// This functions takes all the tokens that have been passed to the macro
/// invocation and performs all the checks that have been implemented in this
/// crate.
pub fn check_macro<Span>(
    ctx: InvocationContext,
    input: Vec<TokenTree<Span>>,
) -> Result<(), Error<Span>>
where
    Span: Copy,
{
    let mut iter = input.into_iter();

    while let Some(head) = iter.next() {
        let TokenTreeKind::Parenthesed(matcher) = head.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::Matcher],
                where_: head.span,
            });
        };

        let matcher = matcher::TokenTree::from_generic(matcher)?;
        let matcher = matcher::Matcher::from_generic(&matcher)?;

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(head.span),
            });
        };

        let TokenTreeKind::Terminal(Terminal::FatArrow) = token.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::Terminal(Terminal::FatArrow)],
                where_: token.span,
            });
        };

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(token.span),
            });
        };

        let TokenTreeKind::CurlyBraced(substitution) = token.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::Transcriber],
                where_: token.span,
            });
        };

        let substitution = substitution::TokenTree::from_generic(substitution)?;
        repetition_stack::check(&matcher, &substitution)?;

        expansion::check_arm(ctx.to_state(), matcher, &substitution)?;

        if let Some(semi) = iter.next() {
            let TokenTreeKind::Terminal(Terminal::Semi) = semi.kind else {
                return Err(Error::ParsingFailed {
                    what: vec![MacroRuleNode::Terminal(Terminal::Semi)],
                    where_: semi.span,
                });
            };
        }
    }

    Ok(())
}

pub(crate) trait Spannable<Span> {
    type Output;

    fn with_span(self, span: Span) -> Self::Output;
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
    /// What kind of token tree is this?
    pub kind: TokenTreeKind<Span>,
    /// Its position in the input code (useful for error message generation).
    pub span: Span,
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

/// Represents the different types of token tree.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenTreeKind<Span> {
    /// A terminal (ie: a leaf tree node).
    Terminal(Terminal),
    /// A sequence of [`TokenTree`] that is delimited by parenthesis.
    Parenthesed(Vec<TokenTree<Span>>),
    /// A sequence of [`TokenTree`] that is delimited by curly brackets.
    CurlyBraced(Vec<TokenTree<Span>>),
}

impl_spannable!(TokenTreeKind<Span> => TokenTree);

/// A terminal symbol.
///
/// # Multi-character operators
///
/// Multi-character operators (`+=`, `->`, ...) must _not_ be split in multiple
/// [`Terminal`]. Any use of the [`check_macro`] function that does not respect
/// this invariant will is subject to unexpected results.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Terminal {
    /// An arrow (`->`).
    Arrow,
    /// A colon (',').
    Colon,
    /// A comma (`,`).
    Comma,
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

    // Currently used keywords
    /// The `as` keyword.
    As,
    /// The `async` keyword.
    Async,
    /// The `await` keyword.
    Await,
    /// The `break` keyword.
    Break,
    /// The `const` keyword.
    Const,
    /// The `continue` keyword.
    Continue,
    /// The `crate` keyword.
    Crate,
    /// The `dyn` keyword.
    Dyn,
    /// The `else` keyword.
    Else,
    /// The `enum` keyword.
    Enum,
    /// The `extern` keyword.
    Extern,
    /// The `false` keyword.
    False,
    /// The `fn` keyword.
    Fn,
    /// The `for` keyword.
    For,
    /// The `if` keyword.
    If,
    /// The `impl` keyword.
    Impl,
    /// The `in` keyword.
    In,
    /// The `let` keyword.
    Let,
    /// The `loop` keyword.
    Loop,
    /// The `match` keyword.
    Match,
    /// The `mod` keyword.
    Mod,
    /// The `move` keyword.
    Move,
    /// The `mut` keyword.
    Mut,
    /// The `pub` keyword.
    Pub,
    /// The `ref` keyword.
    Ref,
    /// The `return` keyword.
    Return,
    /// The `self` keyword.
    Self_,
    /// The `Self` keyword.
    SelfUpper,
    /// The `static` keyword.
    Static,
    /// The `struct` keyword.
    Struct,
    /// The `super` keyword.
    Super,
    /// The `trait` keyword.
    Trait,
    /// The `true` keyword.
    True,
    /// The `type` keyword.
    Type,
    /// The `union` keyword.
    Union,
    /// The `unsafe` keyword.
    Unsafe,
    /// The `use` keyword.
    Use,
    /// The `where` keyword.
    Where,
    /// The `while` keyword.
    While,

    // Keywords reserved for future use
    /// The `abstract` keyword.
    Abstract,
    /// The `become` keyword.
    Become,
    /// The `box` keyword.
    Box,
    /// The `do` keyword.
    Do,
    /// The `final` keyword.
    Final,
    /// The `macro` keyword.
    Macro,
    /// The `override` keyword.
    Override,
    /// The `priv` keyword.
    Priv,
    /// The `try` keyword.
    Try,
    /// The `typeof` keyword.
    Typeof,
    /// The `unsized` keyword.
    Unsized,
    /// The `virtual` keyword.
    Virtual,
    /// The `yield` keyword.
    Yield,
}

impl_spannable!(Terminal => TokenTree);

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
    fn to_state(self) -> State {
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
pub(crate) struct RepetitionQuantifier<Span> {
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

/// Denotes how much times a repetition shall be repeated.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RepetitionQuantifierKind {
    /// Zero or one repetitions (`?` operator).
    ZeroOrOne,
    /// Zero or more repetitions (`*` operator).
    ZeroOrMore,
    /// One or more repetitions (`+` operator).
    OneOrMore,
}

impl_spannable!(RepetitionQuantifierKind => RepetitionQuantifier);

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! check_macro_test {
        (
            $( #[ $meta:meta ] )*
            $test_name:ident {
                #[$kind:ident]
                {
                    $( $tt:tt )*
                }
            }
        ) => {
            $( #[ $meta ] )*
            #[test]
            fn $test_name() {
                let tokens = quote! { $( $tt )* };
                let ctxt = stringify!($kind).parse::<InvocationContext>().expect("Failed to parse `InvocationContext`");

                check_macro(ctxt, tokens).expect("Macro check returned error");
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

    check_macro_test! {
        #[should_panic = "Macro check returned error: UnexpectedEnd { last_token: None }"]
        empty_expr_is_not_an_expr {
            #[expr]
            {
                () => {}
            }
        }
    }
}
