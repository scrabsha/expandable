//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable</code></h1>
//! An attribute-macro based <code>macro_rules!</code> expansion checker.
//! </div>
//!
//! ## Textbook example
//!
//! `rustc` treats macro definitions as some opaque piece of tokens and don't
//! do any check on it. For instance, the following macro definition is valid:
//!
//! ```rust
//! macro_rules! js_concat {
//!     ($left:expr, $right:expr) => {
//!         $left ++ $right
//!     };
//! }
//! ```
//!
//! However, any call to the `js_concat` macro is invalid, as the `++` operator
//! does not exist in Rust. Luckily for us, this crate provides the
//! [`expandable::expr`] macro, that checks that the macro expands to a valid
//! expression. Let's use it on `js_concat`:
//!
//! [`expandable::expr`]: macro@expr
//!
//! ```rust,compile_fail
//! #[expandable::expr]
//! macro_rules! js_concat {
//!     ($left:expr, $right:expr) => {
//!         $left ++ $right
//!     };
//! }
//! ```
//!
//! This emits the following error [^error-message]:
//! ```none
//! error: Potentially invalid expansion. Expected an identifier.
//!  --> tests/ui/fail/js_concat.rs:4:16
//!   |
//! 4 |         $left ++ $right
//!   |                ^
//! ```
//!
//! ## Expansion context
//!
//! Ever wondered what the `expr` of `#[expandable::expr]` stands for?
//!
//! TODO
//!
//! [^error-message]: The Rust grammar is not fully implemented at the moment,
//!     leading to incomplete "expected xxx" list this will be fixed before the
//!     first non-alpha release of this crate.

mod syn_shim;

extern crate proc_macro;

use proc_macro::TokenStream as TokenStream1;
use std::str::FromStr;

use proc_macro2::{Delimiter, Punct, Spacing, Span, TokenStream, TokenTree};
use syn::{
    parse::{Parse, ParseStream},
    Ident,
};

use syn_shim::ItemMacroRules;

use expandable_impl::TokenDescription;

macro_rules! attribute_macro {
    ($name:ident => $variant:ident) => {
        #[proc_macro_attribute]
        pub fn $name(_: TokenStream1, item: TokenStream1) -> TokenStream1 {
            let mut item_ = item.clone();

            let macro_ = match syn::parse2::<ItemMacroRules>(item.into()) {
                Ok(macro_) => macro_,
                Err(e) => return e.to_compile_error().into(),
            };
            let stream = parse_macro_stream(macro_.tokens);

            if let Err(e) =
                expandable_impl::check_macro(expandable_impl::InvocationContext::$variant, stream)
            {
                item_.extend(TokenStream1::from(mk_error_msg(e).into_compile_error()));
                return item_;
            }

            item_
        }
    };
}

attribute_macro!(expr => Expr);
attribute_macro!(item => Item);

fn mk_error_msg(error: expandable_impl::Error<Span>) -> syn::Error {
    let (message, span) = match error {
        expandable_impl::Error::ParsingFailed { where_, .. } => (
            "Failed to parse `macro_rules` body".to_string(),
            Some(where_),
        ),

        expandable_impl::Error::UnexpectedEnd { last_token, .. } => {
            ("Unexpected end of macro invocation".to_string(), last_token)
        }

        expandable_impl::Error::InvalidProducedAst { span, expected, .. } => {
            let expected = expected.iter().map(describe).collect::<Vec<_>>().join(", ");
            (
                format!("Potentially invalid expansion. Expected {expected}."),
                Some(span),
            )
        }

        expandable_impl::Error::UnboundMetavariable { name, where_, .. } => {
            (format!("Unbound metavariable `{name}`"), Some(where_))
        }

        _ => (
            "`expandable` returned an error the expandable macro does not handle (yet)".to_string(),
            None,
        ),
    };

    let span = span.unwrap_or_else(Span::call_site);
    syn::Error::new(span, message)
}

fn describe(descr: &TokenDescription) -> &'static str {
    match descr {
        TokenDescription::Paren => "a parenthesis",
        TokenDescription::Bracket => "a bracket",
        TokenDescription::Brace => "a brace",
        TokenDescription::Invalid => unreachable!(),
        TokenDescription::Ident => "an identifier",
        TokenDescription::Fn => "`fn`",
        TokenDescription::Plus => "`+`",
        TokenDescription::Times => "`*`",
        TokenDescription::Comma => "`,`",
        TokenDescription::Colon => "`:`",
    }
}

fn parse_macro_stream(stream: TokenStream) -> Vec<expandable_impl::TokenTree<Span>> {
    let mut output = Vec::new();
    let iter = stream.into_iter().collect::<Vec<_>>();
    let mut iter = iter.as_slice();

    while let Some((head, mut tail)) = iter.split_first() {
        let mut span = head.span();
        let kind = match head {
            TokenTree::Group(g) => {
                let inner = parse_macro_stream(g.stream());
                match g.delimiter() {
                    Delimiter::Parenthesis => expandable_impl::TokenTreeKind::Parenthesed(inner),
                    Delimiter::Brace => expandable_impl::TokenTreeKind::CurlyBraced(inner),
                    Delimiter::Bracket => todo!("Need some work in the impl crate"),
                    Delimiter::None => todo!("How did we get here?"),
                }
            }

            TokenTree::Ident(id) if id == "fn" => {
                expandable_impl::TokenTreeKind::Terminal(expandable_impl::Terminal::Fn)
            }

            TokenTree::Ident(id) => expandable_impl::TokenTreeKind::Terminal(
                expandable_impl::Terminal::Ident(id.to_string()),
            ),

            TokenTree::Punct(p) => {
                expandable_impl::TokenTreeKind::Terminal(match contiguous_punct(p, tail).as_str() {
                    // FIXME: the following calls to `join` always returns `None`
                    // on stable because it relies on the`proc_macro_span` feature.
                    s if s.starts_with("->") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap_or_else(|| p.span());
                        tail = tail_;
                        expandable_impl::Terminal::Arrow
                    }
                    s if s.starts_with("=>") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap_or_else(|| p.span());
                        tail = tail_;
                        expandable_impl::Terminal::FatArrow
                    }
                    s if s.starts_with(':') => expandable_impl::Terminal::Colon,
                    s if s.starts_with(',') => expandable_impl::Terminal::Comma,
                    s if s.starts_with('$') => expandable_impl::Terminal::Dollar,
                    s if s.starts_with('+') => expandable_impl::Terminal::Plus,
                    s if s.starts_with('?') => expandable_impl::Terminal::QuestionMark,
                    s if s.starts_with(';') => expandable_impl::Terminal::Semi,
                    s if s.starts_with('*') => expandable_impl::Terminal::Times,

                    s => todo!("Unknown start of token: {s}"),
                })
            }

            TokenTree::Literal(_) => todo!(),
        };

        let tree = expandable_impl::TokenTree { kind, span };
        output.push(tree);
        iter = tail;
    }

    fn contiguous_punct(first: &Punct, tail: &[TokenTree]) -> String {
        let mut last_is_joint = true;

        std::iter::once(first)
            .chain(tail.iter().map_while(|tree| match tree {
                TokenTree::Punct(p) => Some(p),
                _ => None,
            }))
            .take_while(|p| {
                let tmp = last_is_joint;
                last_is_joint = p.spacing() == Spacing::Joint;
                tmp
            })
            .map(Punct::as_char)
            .collect()
    }

    output
}

struct InvocationContext(expandable_impl::InvocationContext);

impl Parse for InvocationContext {
    fn parse(input: ParseStream) -> syn::Result<InvocationContext> {
        let ident = input.parse::<Ident>()?;

        expandable_impl::InvocationContext::from_str(&ident.to_string())
            .map(InvocationContext)
            .map_err(|()| {
                syn::Error::new(
                    ident.span(),
                    "Unknown invocation context. Expected `item` or `expr`",
                )
            })
    }
}

#[cfg(test)]
#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.pass("tests/ui/pass/*.rs");
    t.compile_fail("tests/ui/fail/*.rs");
}
