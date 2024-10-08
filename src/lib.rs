//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable</code></h1>
//! An opinionated attribute-macro based <code>macro_rules!</code> expansion
//! checker.
//! </div>
//!
//! <br />
//! <br />
#![doc = include_str!("../doc/00-top-image.md")]
//!
#![doc = include_str!("../doc/01-textbook-example.md")]
//! Luckily for us, this crate provides the [`expandable::expr`] macro, that
//! checks that the macro expands to a valid expression. Let's use it on
//! `js_concat`:
//!
//! ```rust,compile_fail
//! #[expandable::expr]
//! macro_rules! my_vec {
//!   ($($t:expr),*) => {{
//!       let mut buffer = Vec::new();
//!
//!       $(
//!           buffer->push($t);
//!       )*
//!
//!       buffer
//!   }};
//! }
//! ```
//!
//! This emits the following error [^error-message]:
//! ```none
#![doc = include_str!("../tests/ui/fail/my_vec.stderr_nightly")]
//! ```
//! 
//! [^error-message]: The Rust grammar is not fully implemented at the moment,
//!     leading to incomplete "expected xxx" list this will be fixed before the
//!     first non-alpha release of this crate.
//!
//! ## Expansion context
//!
//! Macros can expand to different things depending on where they are called.
//! As a result, `expandable` must know what the macro expands to. To do so,
//! multiple macros are available:
//! - Macros that expand to expressions are checked by [`expandable::expr`],
//! - Macros that expand to items are checked by [`expandable::item`],
//! - Macros that expand to patterns are checked by [`expandable::pat`],
//! - Macros that expand to statements are checked by [`expandable::stmt`],
//! - Macros that expand to types are checked by [`expandable::ty`],
//!
//! [`expandable::expr`]: macro@expr
//! [`expandable::item`]: macro@item
//! [`expandable::pat`]: macro@pat
//! [`expandable::stmt`]: macro@stmt
//! [`expandable::ty`]: macro@ty
#![doc = include_str!("../doc/02-what-can-it-detect.md")]
//!
#![doc = include_str!("../doc/03-opinionated.md")]
//!
#![doc = include_str!("../doc/98-msrv.md")]
//!
#![doc = include_str!("../doc/99-license.md")]

extern crate proc_macro;

use std::{
    collections::BTreeSet,
    fmt::{Display, Formatter},
};

use expandable_impl::{FragmentKind, RepetitionQuantifierKind, Terminal, TokenDescription};
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Delimiter, Spacing, Span, TokenStream, TokenTree};
use syn_shim::ItemMacroRules;

mod syn_shim;

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
attribute_macro!(pat => Pat);
attribute_macro!(stmt => Stmt);
attribute_macro!(ty => Ty);

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
            let expected = rassemble_expected_descrs(expected);
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
                format!(
                    "the repetition used for `{metavariable_name}` ({got_nesting}) is different \
                     from how it is matched ({expected_nesting})."
                ),
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

const MAX_EXPECTED_NUM: usize = 6;

fn rassemble_expected_descrs(expected: Vec<TokenDescription>) -> String {
    // This is a slow path - we are allowed to allocate as much as required.
    // `BtreeSet`s have set properties (no duplicate), and they give us a cool
    // ordering.

    let (mut possible_fragments, mut rest) = (BTreeSet::new(), BTreeSet::new());

    for expected in expected {
        match expected {
            TokenDescription::Fragment(_) => possible_fragments.insert(describe(&expected)),

            // We count an ident (description) as an ident (fragment) so that
            // we avoid printing it twice in the error message.
            TokenDescription::Ident => possible_fragments
                .insert(describe(&TokenDescription::Fragment(FragmentKind::Ident))),

            _ => rest.insert(describe(&expected)),
        };
    }

    let mut expected = possible_fragments.into_iter().collect::<Vec<_>>();
    expected.extend(rest);

    let expected_len = expected.len();

    let (expected, or_n_others) = if expected_len > MAX_EXPECTED_NUM {
        let to_print = expected[0..MAX_EXPECTED_NUM].join(", ");
        let n = expected_len - MAX_EXPECTED_NUM;
        let n_others = format!(" or {n} others");

        (to_print, n_others)
    } else {
        let to_print = expected.join(", ");
        let n_others = String::new();

        (to_print, n_others)
    };

    format!("{expected}{or_n_others}")
}

fn pp_repetition_ops(stack: &[RepetitionQuantifierKind]) -> impl Display + '_ {
    PpRepetitionOps(stack)
}

struct PpRepetitionOps<'a>(&'a [RepetitionQuantifierKind]);

impl Display for PpRepetitionOps<'_> {
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
        TokenDescription::Ident => "an identifier",
        TokenDescription::Literal => "a literal",

        TokenDescription::Plus => "`+`",
        TokenDescription::Minus => "`-`",
        TokenDescription::Star => "`*`",
        TokenDescription::Slash => "`/`",
        TokenDescription::Percent => "`%`",
        TokenDescription::Caret => "`^`",
        TokenDescription::Not => "`!`",
        TokenDescription::And => "`&`",
        TokenDescription::Or => "`|`",
        TokenDescription::AndAnd => "`&&`",
        TokenDescription::OrOr => "`||`",
        TokenDescription::Shl => "`<<`",
        TokenDescription::Shr => "`>>`",
        TokenDescription::PlusEquals => "`+=`",
        TokenDescription::MinusEquals => "`-=`",
        TokenDescription::StarEquals => "`*=`",
        TokenDescription::SlashEquals => "`/=`",
        TokenDescription::PercentEquals => "`%=`",
        TokenDescription::CaretEquals => "`^=`",
        TokenDescription::AndEquals => "`&=`",
        TokenDescription::OrEquals => "`|=`",
        TokenDescription::ShlEquals => "`<<=`",
        TokenDescription::ShrEquals => "`>>=`",
        TokenDescription::Equals => "`=`",
        TokenDescription::EqualsEquals => "`==`",
        TokenDescription::NotEquals => "`!=`",
        TokenDescription::GreaterThan => "`>`",
        TokenDescription::LessThan => "`<`",
        TokenDescription::GreaterThanEquals => "`>=`",
        TokenDescription::LessThanEquals => "`<=`",
        TokenDescription::At => "`@`",
        TokenDescription::Underscore => "`_`",
        TokenDescription::Dot => "`.`",
        TokenDescription::DotDot => "`..`",
        TokenDescription::DotDotDot => "`...`",
        TokenDescription::DotDotEquals => "`..=`",
        TokenDescription::Comma => "`,`",
        TokenDescription::Semicolon => "`;`",
        TokenDescription::Colon => "`:`",
        TokenDescription::ColonColon => "`::`",
        TokenDescription::RightArrow => "`->`",
        TokenDescription::FatArrow => "`=>`",
        TokenDescription::Pound => "`#`",
        TokenDescription::Dollar => "`$`",
        TokenDescription::QuestionMark => "`?`",

        TokenDescription::Fragment(FragmentKind::Expr) => "an expression",
        TokenDescription::Fragment(FragmentKind::Ident) => "an identifier",
        TokenDescription::Fragment(FragmentKind::Item) => "an item",
        TokenDescription::Fragment(FragmentKind::Pat) => "a pattern",

        TokenDescription::Invalid => unreachable!(),

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
                .unwrap_or_else(|| todo!("Unknown token description: {:?}", other));
        }
    }
    .to_string()
}

#[doc = include_str!("../README.md")]
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
                    Delimiter::Bracket => expandable_impl::TokenTreeKind::Bracketed(inner),
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

            TokenTree::Punct(_) => {
                let (span_, terminal, tail_) =
                    parse_punctuation(iter).expect("Failed to parse a checked punctuation");
                span = span_;
                tail = tail_;
                expandable_impl::TokenTreeKind::Terminal(terminal)
            }

            TokenTree::Literal(lit) => {
                expandable_impl::TokenTreeKind::Terminal(Terminal::Literal(lit.to_string()))
            }
        };

        let tree = expandable_impl::TokenTree { kind, span };
        output.push(tree);
        iter = tail;
    }

    output
}

fn parse_punctuation(mut input: &[TokenTree]) -> Option<(Span, Terminal, &[TokenTree])> {
    let mut current = None;
    while let Some((TokenTree::Punct(p), tail)) = input.split_first() {
        let matched = match p.as_char() {
            '+' => Terminal::Plus,
            '-' => Terminal::Minus,
            '*' => Terminal::Star,
            '/' => Terminal::Slash,
            '%' => Terminal::Percent,
            '^' => Terminal::Caret,
            '!' => Terminal::Not,
            '&' => Terminal::And,
            '|' => Terminal::Or,
            '=' => Terminal::Equals,
            '>' => Terminal::GreaterThan,
            '<' => Terminal::LessThan,
            '@' => Terminal::At,
            '_' => Terminal::Underscore,
            '.' => Terminal::Dot,
            ',' => Terminal::Comma,
            ';' => Terminal::Semicolon,
            ':' => Terminal::Colon,
            '#' => Terminal::Pound,
            '$' => Terminal::Dollar,
            '?' => Terminal::QuestionMark,

            _ => break,
        };

        let expand = |span: Span| span.join(p.span()).unwrap_or(span);

        current = match (&current, matched) {
            (None, matched) => Some((p.span(), matched)),
            (Some((span, Terminal::And)), Terminal::And) => Some((expand(*span), Terminal::AndAnd)),
            (Some((span, Terminal::Or)), Terminal::Or) => Some((expand(*span), Terminal::OrOr)),
            (Some((span, Terminal::LessThan)), Terminal::LessThan) => {
                Some((expand(*span), Terminal::Shl))
            }
            (Some((span, Terminal::GreaterThan)), Terminal::GreaterThan) => {
                Some((expand(*span), Terminal::Shr))
            }
            (Some((span, Terminal::Plus)), Terminal::Equals) => {
                Some((expand(*span), Terminal::PlusEquals))
            }
            (Some((span, Terminal::Minus)), Terminal::Equals) => {
                Some((expand(*span), Terminal::MinusEquals))
            }
            (Some((span, Terminal::Star)), Terminal::Equals) => {
                Some((expand(*span), Terminal::StarEquals))
            }
            (Some((span, Terminal::Slash)), Terminal::Equals) => {
                Some((expand(*span), Terminal::SlashEquals))
            }
            (Some((span, Terminal::Percent)), Terminal::Equals) => {
                Some((expand(*span), Terminal::PercentEquals))
            }
            (Some((span, Terminal::Caret)), Terminal::Equals) => {
                Some((expand(*span), Terminal::CaretEquals))
            }
            (Some((span, Terminal::And)), Terminal::Equals) => {
                Some((expand(*span), Terminal::AndEquals))
            }
            (Some((span, Terminal::Or)), Terminal::Equals) => {
                Some((expand(*span), Terminal::OrEquals))
            }
            (Some((span, Terminal::Shl)), Terminal::Equals) => {
                Some((expand(*span), Terminal::ShlEquals))
            }
            (Some((span, Terminal::Shr)), Terminal::Equals) => {
                Some((expand(*span), Terminal::ShrEquals))
            }
            (Some((span, Terminal::Equals)), Terminal::Equals) => {
                Some((expand(*span), Terminal::EqualsEquals))
            }
            (Some((span, Terminal::Not)), Terminal::Equals) => {
                Some((expand(*span), Terminal::NotEquals))
            }
            (Some((span, Terminal::GreaterThan)), Terminal::Equals) => {
                Some((expand(*span), Terminal::GreaterThanEquals))
            }
            (Some((span, Terminal::LessThan)), Terminal::Equals) => {
                Some((expand(*span), Terminal::LessThanEquals))
            }
            (Some((span, Terminal::Dot)), Terminal::Dot) => Some((expand(*span), Terminal::DotDot)),
            (Some((span, Terminal::DotDot)), Terminal::Dot) => {
                Some((expand(*span), Terminal::DotDotDot))
            }
            (Some((span, Terminal::DotDot)), Terminal::Equals) => {
                Some((expand(*span), Terminal::DotDotEquals))
            }
            (Some((span, Terminal::Colon)), Terminal::Colon) => {
                Some((expand(*span), Terminal::ColonColon))
            }
            (Some((span, Terminal::Minus)), Terminal::GreaterThan) => {
                Some((expand(*span), Terminal::RightArrow))
            }
            (Some((span, Terminal::Equals)), Terminal::GreaterThan) => {
                Some((expand(*span), Terminal::FatArrow))
            }

            _ => break,
        };

        input = tail;

        if p.spacing() == Spacing::Alone {
            break;
        }
    }

    current.map(|(span, terminal)| (span, terminal, input))
}

#[cfg(test)]
#[test]
fn ui() {
    setup_channel_dependant_stderrs();

    let t = trybuild::TestCases::new();
    t.pass("tests/ui/pass/*.rs");
    t.compile_fail("tests/ui/fail/*.rs");

    drop(t);

    cleanup_channel_dependant_stderrs();
}

#[cfg(test)]
const CHANNEL_DEPENDANT_STDERRS: [&str; 2] = ["bad_range_pattern", "my_vec"];

#[cfg(test)]
fn setup_channel_dependant_stderrs() {
    // The error messages of `expandable` depend on the channel that is used
    // when compiling the crate, as we use the nightly-only `Span::join` method.
    //
    // We circumvent this by checking against a different stderr depending on
    // the channel that is used when the macro is compiled.

    use std::fs;

    for (original, link) in channel_dependant_stderrs() {
        let _ = fs::remove_file(&link);
        fs::hard_link(original, link).unwrap();
    }
}

#[cfg(test)]
fn cleanup_channel_dependant_stderrs() {
    use std::fs;

    for (_, link) in channel_dependant_stderrs() {
        fs::remove_file(link).unwrap();
    }
}

#[cfg(test)]
fn channel_dependant_stderrs() -> Vec<(String, String)> {
    use rustc_version::Channel;

    CHANNEL_DEPENDANT_STDERRS
        .into_iter()
        .map(|unstable_stderr| {
            let channel = rustc_version::version_meta().unwrap().channel;
            let suffix = match channel {
                Channel::Nightly | Channel::Dev => "nightly",
                Channel::Beta | Channel::Stable => "stable",
            };

            let in_path = format!("tests/ui/fail/{unstable_stderr}.stderr_{suffix}");
            let out_path = format!("tests/ui/fail/{unstable_stderr}.stderr");

            (in_path, out_path)
        })
        .collect()
}
