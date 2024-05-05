// Architectural invariant: this module contains basic types that allow to parse
// the Rust language.

use std::{
    hash::{Hash, Hasher},
    ptr,
};

use rust_grammar_dpdfa::RustParser;
pub(crate) use rust_grammar_dpdfa::Transition;

use crate::{FragmentKind, Terminal};

#[derive(Clone, Debug)]
pub(crate) struct DynamicState<Span>
where
    Span: 'static,
{
    pub(crate) state: RustParser<Span>,
}

impl<Span> PartialEq for DynamicState<Span>
where
    Span: 'static,
{
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other) || self.state == other.state
    }
}

impl<Span> Eq for DynamicState<Span> {}

impl<Span> Hash for DynamicState<Span> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.state.hash(state);
    }
}

impl<Span> DynamicState<Span>
where
    Span: Copy + 'static,
{
    pub(crate) fn item() -> DynamicState<Span> {
        DynamicState {
            state: RustParser::item(),
        }
    }

    pub(crate) fn expr() -> DynamicState<Span> {
        DynamicState {
            state: RustParser::expr(),
        }
    }

    pub(crate) fn pat() -> DynamicState<Span> {
        DynamicState {
            state: RustParser::pat(),
        }
    }

    pub(crate) fn accept_fragment(
        self,
        fragment: FragmentKind,
        span: Span,
    ) -> Result<(DynamicState<Span>, Transition), (Span, Vec<TokenDescription>)> {
        self.accept(TokenDescription::Fragment(fragment), span)
    }

    pub(crate) fn accept(
        mut self,
        descr: TokenDescription,
        span: Span,
    ) -> Result<(DynamicState<Span>, Transition), (Span, Vec<TokenDescription>)> {
        let descr = rust_grammar_dpdfa::TokenDescription::from(descr);
        let trans = self.state.step(descr, span).map_err(|(s, e)| {
            let e = e.into_iter().flat_map(TokenDescription::try_from).collect();
            (s, e)
        })?;

        Ok((self, trans))
    }

    pub(crate) fn is_accepting(&mut self) -> Result<(), Option<(Span, Vec<TokenDescription>)>> {
        self.state.finish().map_err(|e| {
            e.map(|(s, e)| {
                let e = e.into_iter().flat_map(TokenDescription::try_from).collect();
                (s, e)
            })
        })
    }
}

macro_rules! token_description {
    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $(
                $( #[$meta_:meta] )*
                $pattern:pat => $variant:ident
            ),* $(,)?
        }
    ) => {
        $( #[$meta] )*
        #[non_exhaustive]
        $vis enum $name {
            /// An opening parenthesis.
            LParen,
            /// A closing parenthesis.
            RParen,
            /// An opening bracket.
            LBracket,
            /// A closing bracket.
            RBracket,
            /// An opening brace.
            LBrace,
            /// A closing brace.
            RBrace,

            /// A fragment.
            Fragment(FragmentKind),

            /// An invalid token.
            Invalid,

            $(
                $( #[$meta_] )*
                $variant
            ),*
        }

        impl From<&Terminal> for $name {
            fn from(value: &Terminal) -> $name {
                match value {
                    $( $pattern => $name::$variant, )*
                }
            }
        }

        impl From<$name> for rust_grammar_dpdfa::TokenDescription {
            fn from(descr: $name) -> rust_grammar_dpdfa::TokenDescription {
                match descr {
                    $(
                        $name::$variant => rust_grammar_dpdfa::TokenDescription::$variant,
                    )*

                    $name::LParen => rust_grammar_dpdfa::TokenDescription::LParen,
                    $name::RParen => rust_grammar_dpdfa::TokenDescription::RParen,
                    $name::LBracket => rust_grammar_dpdfa::TokenDescription::LBracket,
                    $name::RBracket => rust_grammar_dpdfa::TokenDescription::RBracket,
                    $name::LBrace => rust_grammar_dpdfa::TokenDescription::LBrace,
                    $name::RBrace => rust_grammar_dpdfa::TokenDescription::RBrace,

                    $name::Fragment(FragmentKind::Expr) => rust_grammar_dpdfa::TokenDescription::FragmentExpr,
                    $name::Fragment(FragmentKind::Ident) => rust_grammar_dpdfa::TokenDescription::FragmentIdent,
                    $name::Fragment(FragmentKind::Item) => rust_grammar_dpdfa::TokenDescription::FragmentItem,
                    $name::Fragment(FragmentKind::Pat) => rust_grammar_dpdfa::TokenDescription::FragmentPat,

                    $name::Invalid => unreachable!(),
                }
            }
        }

        impl TryFrom<rust_grammar_dpdfa::TokenDescription> for $name {
            type Error = ();

            fn try_from(descr: rust_grammar_dpdfa::TokenDescription) -> Result<$name, ()> {
                Ok(
                    match descr {
                        $(
                            rust_grammar_dpdfa::TokenDescription::$variant => $name::$variant,
                        )*

                        rust_grammar_dpdfa::TokenDescription::LParen => TokenDescription::LParen,
                        rust_grammar_dpdfa::TokenDescription::RParen => TokenDescription::RParen,
                        rust_grammar_dpdfa::TokenDescription::LBracket => TokenDescription::LBracket,
                        rust_grammar_dpdfa::TokenDescription::RBracket => TokenDescription::RBracket,
                        rust_grammar_dpdfa::TokenDescription::LBrace => TokenDescription::LBrace,
                        rust_grammar_dpdfa::TokenDescription::RBrace => TokenDescription::RBrace,

                        // TODO: support block
                        // rust_grammar_dpdfa::TokenDescription::FragmentBlock => TokenDescription::Fragment(FragmentKind::Block),
                        rust_grammar_dpdfa::TokenDescription::FragmentExpr => TokenDescription::Fragment(FragmentKind::Expr),
                        rust_grammar_dpdfa::TokenDescription::FragmentIdent => TokenDescription::Fragment(FragmentKind::Ident),
                        rust_grammar_dpdfa::TokenDescription::FragmentItem => TokenDescription::Fragment(FragmentKind::Item),
                        rust_grammar_dpdfa::TokenDescription::FragmentPat => TokenDescription::Fragment(FragmentKind::Pat),
                        // TODO: support lifetime
                        // rust_grammar_dpdfa::TokenDescription::FragmentLifetime => TokenDescription::Fragment(FragmentKind::Lifetime),
                        // TODO: support literal
                        // rust_grammar_dpdfa::TokenDescription::FragmentLiteral => TokenDescription::Fragment(FragmentKind::Literal),
                        // TODO: support meta
                        // rust_grammar_dpdfa::TokenDescription::FragmentMeta => TokenDescription::Fragment(FragmentKind::Meta),
                        // TODO: support stmt
                        // rust_grammar_dpdfa::TokenDescription::FragmentStmt => TokenDescription::Fragment(FragmentKind::Stmt),
                        // TODO: support TT
                        // rust_grammar_dpdfa::TokenDescription::FragmentTT => TokenDescription::Fragment(FragmentKind::TT),
                        // TODO: support ty
                        // rust_grammar_dpdfa::TokenDescription::FragmentTy => TokenDescription::Fragment(FragmentKind::Ty),
                        // TODO: support vis
                        // rust_grammar_dpdfa::TokenDescription::FragmentVis => TokenDescription::Fragment(FragmentKind::Vis),


                        _ => return Err(()),
                    }

                )
            }
        }
    };
}

token_description! {
    /// Describes a [`Terminal`].
    ///
    /// This allows library user to accurately report what kind of token is
    /// expected.
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum TokenDescription {
        /// An identifier. Does not include keywords.
        Terminal::Ident(_) => Ident,

        // Keywords
        /// The `as` keyword.
        Terminal::As => As,
        /// The `async` keyword.
        Terminal::Async => Async,
        /// The `await` keyword.
        Terminal::Await => Await,
        /// The `break` keyword.
        Terminal::Break => Break,
        /// The `const` keyword.
        Terminal::Const => Const,
        /// The `continue` keyword.
        Terminal::Continue => Continue,
        /// The `crate` keyword.
        Terminal::Crate => Crate,
        /// The `dyn` keyword.
        Terminal::Dyn => Dyn,
        /// The `else` keyword.
        Terminal::Else => Else,
        /// The `enum` keyword.
        Terminal::Enum => Enum,
        /// The `extern` keyword.
        Terminal::Extern => Extern,
        /// The `false` keyword.
        Terminal::False => False,
        /// The `fn` keyword.
        Terminal::Fn => Fn,
        /// The `for` keyword.
        Terminal::For => For,
        /// The `if` keyword.
        Terminal::If => If,
        /// The `impl` keyword.
        Terminal::Impl => Impl,
        /// The `in` keyword.
        Terminal::In => In,
        /// The `let` keyword.
        Terminal::Let => Let,
        /// The `loop` keyword.
        Terminal::Loop => Loop,
        /// The `match` keyword.
        Terminal::Match => Match,
        /// The `mod` keyword.
        Terminal::Mod => Mod,
        /// The `move` keyword.
        Terminal::Move => Move,
        /// The `mut` keyword.
        Terminal::Mut => Mut,
        /// The `pub` keyword.
        Terminal::Pub => Pub,
        /// The `ref` keyword.
        Terminal::Ref => Ref,
        /// The `return` keyword.
        Terminal::Return => Return,
        /// The `self` keyword.
        Terminal::Self_ => Self_,
        /// The `Self` keyword.
        Terminal::SelfUpper => SelfUpper,
        /// The `static` keyword.
        Terminal::Static => Static,
        /// The `struct` keyword.
        Terminal::Struct => Struct,
        /// The `super` keyword.
        Terminal::Super => Super,
        /// The `trait` keyword.
        Terminal::Trait => Trait,
        /// The `true` keyword.
        Terminal::True => True,
        /// The `type` keyword.
        Terminal::Type => Type,
        /// The `union` keyword.
        Terminal::Union => Union,
        /// The `unsafe` keyword.
        Terminal::Unsafe => Unsafe,
        /// The `use` keyword.
        Terminal::Use => Use,
        /// The `where` keyword.
        Terminal::Where => Where,
        /// The `while` keyword.
        Terminal::While => While,
        /// The `async` keyword.
        Terminal::Yield => Yield,

        /// The `abstract` keyword.
        Terminal::Abstract => Abstract,
        /// The `become` keyword.
        Terminal::Become => Become,
        /// The `box` keyword.
        Terminal::Box => Box,
        /// The `do` keyword.
        Terminal::Do => Do,
        /// The `final` keyword.
        Terminal::Final => Final,
        /// The `macro` keyword.
        Terminal::Macro => Macro,
        /// The `override` keyword.
        Terminal::Override => Override,
        /// The `priv` keyword.
        Terminal::Priv => Priv,
        /// The `try` keyword.
        Terminal::Try => Try,
        /// The `typeof` keyword.
        Terminal::Typeof => Typeof,
        /// The `unsized` keyword.
        Terminal::Unsized => Unsized,
        /// The `virtual` keyword.
        Terminal::Virtual => Virtual,

        // Literals
        /// A literal
        Terminal::Literal(_) => Literal,

        // Punctuates
        /// A plus (`+`).
        Terminal::Plus => Plus,
        /// A minus (`-`).
        Terminal::Minus => Minus,
        /// A star (`*`).
        Terminal::Star => Star,
        /// A slash (`/`).
        Terminal::Slash => Slash,
        /// A percent sign (`%`).
        Terminal::Percent => Percent,
        /// A caret (`^`).
        Terminal::Caret => Caret,
        /// A not (`!`).
        Terminal::Not => Not,
        /// An ampersand (`&`).
        Terminal::And => And,
        /// An or (`|`).
        Terminal::Or => Or,
        /// A lazy boolean and (`&&`).
        Terminal::AndAnd => AndAnd,
        /// A lazy boolean or (`||`).
        Terminal::OrOr => OrOr,
        /// A shift left (`<<`).
        Terminal::Shl => Shl,
        /// A shift right (`>>`).
        Terminal::Shr => Shr,
        /// A plus-equals (`+=`).
        Terminal::PlusEquals => PlusEquals,
        /// A minus-equals (`-=`).
        Terminal::MinusEquals => MinusEquals,
        /// A star-equals (`*=`).
        Terminal::StarEquals => StarEquals,
        /// A slash-equals (`/=`).
        Terminal::SlashEquals => SlashEquals,
        /// A percent-equals (`%=`).
        Terminal::PercentEquals => PercentEquals,
        /// A caret-equals (`^=`).
        Terminal::CaretEquals => CaretEquals,
        /// An and-equals (`&=`).
        Terminal::AndEquals => AndEquals,
        /// An or-equals (`|=`).
        Terminal::OrEquals => OrEquals,
        /// A shift-left-equals (`<<=`).
        Terminal::ShlEquals => ShlEquals,
        /// A shift-right-equals (`>>=`).
        Terminal::ShrEquals => ShrEquals,
        /// An equals (`=`).
        Terminal::Equals => Equals,
        /// An equals equals (`==`).
        Terminal::EqualsEquals => EqualsEquals,
        /// A not equals (`!=`).
        Terminal::NotEquals => NotEquals,
        /// A greater than (`>`).
        Terminal::GreaterThan => GreaterThan,
        /// A left chevron (`<`).
        Terminal::LessThan => LessThan,
        /// A greater than equals (`>=`).
        Terminal::GreaterThanEquals => GreaterThanEquals,
        /// A less than equals (`<=`).
        Terminal::LessThanEquals => LessThanEquals,
        /// An at (`@`).
        Terminal::At => At,
        /// An underscore (`_`).
        Terminal::Underscore => Underscore,
        /// A dot (`.`).
        Terminal::Dot => Dot,
        /// A dot dot (`..`).
        Terminal::DotDot => DotDot,
        /// A dot dot dot (`...`).
        Terminal::DotDotDot => DotDotDot,
        /// A dot dot equals (`..=`).
        Terminal::DotDotEquals => DotDotEquals,
        /// A comma (`,`).
        Terminal::Comma => Comma,
        /// A semicolon (`;`).
        Terminal::Semicolon => Semicolon,
        /// A colon (`:`).
        Terminal::Colon => Colon,
        /// A colon colon (`::`).
        Terminal::ColonColon => ColonColon,
        /// An arrow (`->`).
        Terminal::RightArrow => RightArrow,
        /// A fat arrow (`=>`).
        Terminal::FatArrow => FatArrow,
        /// The pound sign (`#`).
        Terminal::Pound => Pound,
        /// The dollar sign (`$`).
        Terminal::Dollar => Dollar,
        /// A question mark (`?`).
        Terminal::QuestionMark => QuestionMark,
    }
}
