//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable</code></h1>
//! An attribute-macro based <code>macro_rules!</code> expansion checker.
//! </div>
//!
//! ## Example
//!
//! Let's consider the following Rust code:
//!
//! ```rust,compile_fail
//! use expandable::expandable;
//! #[expandable(expr)]
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

extern crate proc_macro;

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Delimiter, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};
use std::str::FromStr;
use syn::parse_macro_input;
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Error, Ident, ItemMacro,
};
use expandable_impl::TokenDescription;

#[proc_macro_attribute]
pub fn expandable(attrs: TokenStream1, item: TokenStream1) -> TokenStream1 {
    let item_ = item.clone();
    let ctx = parse_macro_input!(attrs as InvocationContext);
    let macro_ = parse_macro_input!(item as ItemMacro);

    // We need to ensure that it is a macro_rule thing.
    if macro_.ident.is_none() {
        return quote_spanned! { macro_.mac.path.span() => compile_error!("Expected a `macro_rules!` macro")}
            .into();
    }

    let stream = parse_macro_stream(macro_.mac.tokens);

    if let Err(e) = expandable_impl::check_macro(ctx.0, stream) {
        // We found an error. Yay!!
        return mk_error_msg(item_, e);
    }

    item_
}

fn mk_error_msg(mut item: TokenStream1, error: expandable_impl::Error<Span>) -> TokenStream1 {
    let compile_error = match error {
        expandable_impl::Error::ParsingFailed { where_, .. } => quote_spanned! {
            where_ => compile_error!("Failed to parse `macro_rules` body");
        },

        expandable_impl::Error::UnexpectedEnd {
            last_token: Some(span),
            ..
        } => quote_spanned! {
            span => compile_error!("Unexpected end of macro invocation");
        },

        expandable_impl::Error::UnexpectedEnd { .. } => {
            quote! {
                compile_error!("Unexpected end of macro invocation");
            }
        }

        expandable_impl::Error::InvalidProducedAst { span, expected, .. } => {
            let expected = expected.iter().map(describe).collect::<Vec<_>>().join(", ");
            quote_spanned! {
                // TODO: name what is expected
                span => compile_error!(concat!("Potentially invalid expansion. Expected ", #expected, "."));
            }
        },

        expandable_impl::Error::UnboundMetavariable { name, where_, .. } => quote_spanned! {
            where_ => compile_error!("Unbound metavariable `{}`", #name);
        },

        _ => {
            quote! {
                compile_error!("`expandable` returned an error the expandable macro does not handle (yet)");
            }
        }
    };

    item.extend(TokenStream1::from(compile_error));

    item
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
                    s if s.starts_with("->") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap();
                        tail = tail_;
                        expandable_impl::Terminal::Arrow
                    }
                    s if s.starts_with("=>") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap();
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
                Error::new(
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
