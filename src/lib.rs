//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable</code></h1>
//! An opinionated attribute-macro based <code>macro_rules!</code> expansion
//! checker.
//! </div>
//!
//! <br />
//! <br />
//!
//! <p style="text-align:center" align="center">
//! <img src="https://cdn.githubraw.com/scrabsha/expendable/main/assets/top_image.png"
//!      width=70%
//!      style="text-align:center"
//!      alt="Sylvester Stallone in Rambo: First Blood Part II. The image has been edited such that his two hands are thumbsup-ing. His body is covered with dust and sweat and a bit of blood (not his). At the bottom of the image is written 'POV: #[expandable::expr] stops complaining'." />
//! </p>
//!
//! ## Textbook example
//!
//! `rustc` treats macro definitions as some opaque piece of tokens and don't
//! do any check on them. For instance, the following macro definition is valid:
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
#![doc = include_str!("../tests/ui/fail/js_concat.stderr")]
//! ```
//!
//! ## Expansion context
//!
//! Macros can expand to different things depending on where they are called.
//! As a result, `expandable` must know what the macro expands to. To do so,
//! multiple macros are available:
//! - Macros that expand to expressions are checked by [`expandable::expr`],
//! - Macros that expand to items are checked by [`expandable::item`],
//! - TODO: pattern, statements, type.
//!
//! [`expandable::expr`]: macro@expr
//! [`expandable::item`]: macro@item
//!
//! ## What can it detect?
//!
//! This section briefly describes what the macros can detect and what they
//! can't. All the terminology used in this section is taken from the
//! [Ferrocene Language Specification][ferrocene-spec].
//!
//! This crate aims to detect all the things that are not considered by `rustc`
//! as an invalid macro definition. It may or may not detect what `rustc`
//! already detects. Some errors can't be detected because they are not
//! possible to detect in the first place.
//!
//! [ferrocene-spec]: https://spec.ferrocene.dev/
//!
//! ### Invalid matcher
//!
//! `rustc` is quite good at detecting invalid matchers. This crate deliberately
//! does not try to compete with it. Whether if this crate returns an error on
//! an invalid matcher is left unspecified.
//!
//! ### Invalid transcription
//!
//! Some macros can't be expanded because they don't respect the rules for
//! a transcription to happen. For instance, the following macro exhibits
//! a transcription error:
//!
//! ```rust
//! macro_rules! invalid_expansion {
//!   ( $( $a:ident )? ) => { $a }
//! }
//! ```
//!
//! In this example, the repetition nesting of `$a` used in the macro
//! transcription does not match the repetition nesting of `$a` defined in the
//! matcher.
//!
//! This crate aims to detect all the possible invalid transcription.
//!
//! ### Invalid produced AST
//!
//! Some macro expand correctly (ie: they respect the transcription rules), but
//! the produced AST is invalid. For instance, the following macro expands to
//! an invalid AST:
//!
//! ```rust
//! macro_rules! invalid_expansion {
//!     () => { * }
//! }
//! ```
//!
//! (`*` is an invalid item, an invalid expression, an invalid pattern, an
//! invalid statement and an invalid type -- there is no context in which it can
//! be called).
//!
//! `rustc` doesn't (and can't) detect any potentially invalid AST. This is the
//! _raison d'être_ of this crate.
//!
//! ## Opinionated?
//!
//! In the general case, proving that a macro is well-formed is impossible. This
//! crates has to make assumptions about the macros it is checking. This section
//! lists them and gives a short rationale of why they are needed.
//!
//! ### No recursive macro definition
//!
//! This crate assumes that the macro expansion does not contain any macro
//! definition. For instance, the following macro definition can't be checked
//! with `expandable`:
//!
//! ```rust
//! macro_rules! foo {
//!     () => {
//!         macro_rules! bar {
//!             () => { 42 }
//!         }
//!     }
//! }
//! ```
//!
//! This limitation is caused by the fact that it's impossible to tell if a
//! sequence of tokens is a macro definition or not. For instance, the
//! `macro_rules` token may be passed by the user when invoking the macro.
//! Having no guarantee of what's a macro and what is not makes it impossible
//! to ensure that a metavariable is properly defined.
//!
//! ### No macro call (for now)
//!
//! This crate assumes that _one expansion_ of a macro works. It does not check
//! that the recursive expansion of the macro works. It checks that the macro
//! invocation itself is legal, but don't check that it matches any rule.
//!
//! This is caused by the fact that other macros may add more constrains on the
//! macro invocation. _This requirement may be lifted in the future for macros
//! that call themselves._
//!
//! ### The repetition stack must match
//!
//! This crate requires a metavariable indication used in a transcriber to have
//! the same repetition stack as the corresponding metavariable that appears in
//! the matcher.
//!
//! For instance, the following code is rejected by `expandable`:
//!
//! ```rust,compile_fail
//! #[expandable::expr]
//! macro_rules! fns {
//!     ($($a:ident)*) => {
//!         $(
//!             fn $a() {}
//!         )+
//!     }
//! }
//! ```
//!
//! ## Minimal Supported Rust Version (MSRV), syntax support and stability
//!
//! _`expandable` supports Rust 1.65 and above. Bumping the MSRV is
//! considered a breaking change._
//!
//! Note that the embedded parser will support syntax that was introduced
//! _after_ Rust 1.65.
//!
//! Adding support for newer syntax is _not_ considered a breaking change.
//!
//! The error messages generated by `expandable` are not stable and may change
//! without notice.
//!
//! Any change that may trigger errors on previously accepted code is considered
//! a breaking change.
//!
//! [^error-message]: The Rust grammar is not fully implemented at the moment,
//!     leading to incomplete "expected xxx" list this will be fixed before the
//!     first non-alpha release of this crate.

mod syn_shim;

extern crate proc_macro;

use proc_macro::TokenStream as TokenStream1;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use proc_macro2::{Delimiter, Punct, Spacing, Span, TokenStream, TokenTree};
use syn::{
    parse::{Parse, ParseStream},
    Ident,
};

use syn_shim::ItemMacroRules;

use expandable_impl::{RepetitionQuantifierKind, Terminal, TokenDescription};

macro_rules! attribute_macro {
    ($name:ident => $variant:ident) => {
        #[doc = concat!("Checks that a macro expands to a valid ", stringify!($name), ".")]
        ///
        /// *Refer to the [crate-level documentation][crate] for more.*
        #[proc_macro_attribute]
        pub fn $name(_: TokenStream1, item: TokenStream1) -> TokenStream1 {
            expandable_inner(expandable_impl::InvocationContext::$variant, item)
        }
    };
}

attribute_macro!(expr => Expr);
attribute_macro!(item => Item);

fn expandable_inner(ctx: expandable_impl::InvocationContext, item: TokenStream1) -> TokenStream1 {
    let mut item_ = item.clone();

    let macro_ = match syn::parse2::<ItemMacroRules>(item.into()) {
        Ok(macro_) => macro_,
        Err(e) => return e.to_compile_error().into(),
    };
    let input = parse_macro_stream(macro_.tokens);

    if let Err(e) = expandable_impl::check_macro(ctx, input) {
        item_.extend(TokenStream1::from(mk_error_msg(e).into_compile_error()));
        return item_;
    }

    item_
}

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

        expandable_impl::Error::InvalidRepetitionNesting {
            metavariable_name,
            usage_span,
            expected_nesting,
            got_nesting,
            ..
        } => {
            let expected_nesting = pp_repetition_ops(&expected_nesting);
            let got_nesting = pp_repetition_ops(&got_nesting);

            (
                format!("the repetition used for `{metavariable_name}` ({got_nesting}) is different from how it is matched ({expected_nesting})."),
                Some(usage_span),
            )
        }

        _ => (
            "`expandable` returned an error the expandable macro does not handle (yet)".to_string(),
            None,
        ),
    };

    let span = span.unwrap_or_else(Span::call_site);
    syn::Error::new(span, message)
}

fn pp_repetition_ops(stack: &[RepetitionQuantifierKind]) -> impl Display + '_ {
    PpRepetitionOps(stack)
}

struct PpRepetitionOps<'a>(&'a [RepetitionQuantifierKind]);

impl<'a> Display for PpRepetitionOps<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            write!(f, "no repetition")
        } else {
            write!(f, "`")?;
            self.0
                .iter()
                .try_for_each(|q| write!(f, "{}", quantifier_to_char(*q)))?;
            write!(f, "`")
        }
    }
}

fn quantifier_to_char(q: RepetitionQuantifierKind) -> char {
    match q {
        RepetitionQuantifierKind::ZeroOrOne => '?',
        RepetitionQuantifierKind::ZeroOrMore => '*',
        RepetitionQuantifierKind::OneOrMore => '+',
    }
}

// TODO: we need to move the TokenTree -> TokenTree (lmao funny) to another
// module so that we don't scare newcomers too much.
// TODO: also we should use a hashmap or something :woman_shrugging:.
const DESCRS_AND_STRS: &[(TokenDescription, &str)] = &[
    (TokenDescription::As, "as"),
    (TokenDescription::Async, "async"),
    (TokenDescription::Await, "await"),
    (TokenDescription::Break, "break"),
    (TokenDescription::Const, "const"),
    (TokenDescription::Continue, "continue"),
    (TokenDescription::Crate, "crate"),
    (TokenDescription::Dyn, "dyn"),
    (TokenDescription::Else, "else"),
    (TokenDescription::Enum, "enum"),
    (TokenDescription::Extern, "extern"),
    (TokenDescription::False, "false"),
    (TokenDescription::Fn, "fn"),
    (TokenDescription::For, "for"),
    (TokenDescription::If, "if"),
    (TokenDescription::Impl, "impl"),
    (TokenDescription::In, "in"),
    (TokenDescription::Let, "let"),
    (TokenDescription::Loop, "loop"),
    (TokenDescription::Match, "match"),
    (TokenDescription::Mod, "mod"),
    (TokenDescription::Move, "move"),
    (TokenDescription::Mut, "mut"),
    (TokenDescription::Pub, "pub"),
    (TokenDescription::Ref, "ref"),
    (TokenDescription::Return, "return"),
    (TokenDescription::Self_, "self"),
    (TokenDescription::SelfUpper, "Self"),
    (TokenDescription::Static, "static"),
    (TokenDescription::Struct, "struct"),
    (TokenDescription::Super, "super"),
    (TokenDescription::Trait, "trait"),
    (TokenDescription::True, "true"),
    (TokenDescription::Type, "type"),
    (TokenDescription::Union, "union"),
    (TokenDescription::Unsafe, "unsafe"),
    (TokenDescription::Use, "use"),
    (TokenDescription::Where, "where"),
    (TokenDescription::While, "while"),
];

const TERMS_AND_STRS: &[(Terminal, &str)] = &[
    (Terminal::As, "as"),
    (Terminal::Async, "async"),
    (Terminal::Await, "await"),
    (Terminal::Break, "break"),
    (Terminal::Const, "const"),
    (Terminal::Continue, "continue"),
    (Terminal::Crate, "crate"),
    (Terminal::Dyn, "dyn"),
    (Terminal::Else, "else"),
    (Terminal::Enum, "enum"),
    (Terminal::Extern, "extern"),
    (Terminal::False, "false"),
    (Terminal::Fn, "fn"),
    (Terminal::For, "for"),
    (Terminal::If, "if"),
    (Terminal::Impl, "impl"),
    (Terminal::In, "in"),
    (Terminal::Let, "let"),
    (Terminal::Loop, "loop"),
    (Terminal::Match, "match"),
    (Terminal::Mod, "mod"),
    (Terminal::Move, "move"),
    (Terminal::Mut, "mut"),
    (Terminal::Pub, "pub"),
    (Terminal::Ref, "ref"),
    (Terminal::Return, "return"),
    (Terminal::Self_, "self"),
    (Terminal::SelfUpper, "Self"),
    (Terminal::Static, "static"),
    (Terminal::Struct, "struct"),
    (Terminal::Super, "super"),
    (Terminal::Trait, "trait"),
    (Terminal::True, "true"),
    (Terminal::Type, "type"),
    (Terminal::Union, "union"),
    (Terminal::Unsafe, "unsafe"),
    (Terminal::Use, "use"),
    (Terminal::Where, "where"),
    (Terminal::While, "while"),
];

fn describe(descr: &TokenDescription) -> String {
    match descr {
        TokenDescription::LParen => "a `(`",
        TokenDescription::RParen => "a `)`",
        TokenDescription::LBracket => "a `[`",
        TokenDescription::RBracket => "a `]`",
        TokenDescription::LBrace => "a `{`",
        TokenDescription::RBrace => "a `}`",
        TokenDescription::Invalid => unreachable!(),
        TokenDescription::Ident => "an identifier",
        TokenDescription::Plus => "`+`",
        TokenDescription::Minus => "`-`",
        TokenDescription::Times => "`*`",
        TokenDescription::Comma => "`,`",
        TokenDescription::Colon => "`:`",
        TokenDescription::Semi => "`;`",
        TokenDescription::Arrow => "`->`",
        TokenDescription::FatArrow => "`=>`",
        TokenDescription::QuestionMark => "`?`",
        TokenDescription::Dollar => "`$`",
        TokenDescription::Equal => "`=`",
        TokenDescription::EqualEqual => "`==`",
        TokenDescription::Literal => "a literal",

        other => {
            return DESCRS_AND_STRS
                .iter()
                .find_map(|(k, v)| {
                    if k == other {
                        Some(format!("`{v}`"))
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| todo!("Unknown token description: {:?}", other))
        }
    }
    .to_string()
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

            TokenTree::Ident(id) => {
                let kw = TERMS_AND_STRS
                    .iter()
                    .find_map(|(k, v)| if v == &id.to_string() { Some(k) } else { None })
                    .cloned();

                let terminal = match kw {
                    Some(kw) => kw,
                    None => Terminal::Ident(id.to_string()),
                };

                expandable_impl::TokenTreeKind::Terminal(terminal)
            }

            TokenTree::Punct(p) => {
                expandable_impl::TokenTreeKind::Terminal(match contiguous_punct(p, tail).as_str() {
                    // FIXME: the following calls to `join` always returns `None`
                    // on stable because it relies on the`proc_macro_span` feature.
                    s if s.starts_with("->") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap_or_else(|| p.span());
                        tail = tail_;
                        Terminal::Arrow
                    }
                    s if s.starts_with("=>") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap_or_else(|| p.span());
                        tail = tail_;
                        Terminal::FatArrow
                    }

                    s if s.starts_with("==") => {
                        let (last, tail_) = tail.split_first().unwrap();
                        span = span.join(last.span()).unwrap_or_else(|| p.span());
                        tail = tail_;
                        Terminal::EqualEqual
                    }

                    s if s.starts_with('=') => Terminal::EqualEqual,
                    s if s.starts_with(':') => Terminal::Colon,
                    s if s.starts_with(',') => Terminal::Comma,
                    s if s.starts_with('$') => Terminal::Dollar,
                    s if s.starts_with('+') => Terminal::Plus,
                    s if s.starts_with('-') => Terminal::Minus,
                    s if s.starts_with('?') => Terminal::QuestionMark,
                    s if s.starts_with(';') => Terminal::Semi,
                    s if s.starts_with('*') => Terminal::Times,

                    s => todo!("Unknown start of token: {s}"),
                })
            }

            TokenTree::Literal(lit) => {
                expandable_impl::TokenTreeKind::Terminal(Terminal::Literal(lit.to_string()))
            }
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
