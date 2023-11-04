// Architectural invariant: this module contains basic types that allow to parse
// the Rust language.

use smallvec::{smallvec, SmallVec};

use crate::{FragmentKind, Terminal};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct DynamicState {
    pub(crate) state: State,
    pub(crate) stack: SmallVec<[StackSymbol; 16]>,
}

impl DynamicState {
    pub(crate) fn accept_fragment(
        self,
        fragment: FragmentKind,
    ) -> Result<DynamicState, Vec<TokenDescription>> {
        self.accept(TokenDescription::Fragment(fragment))
    }

    pub(crate) fn accept(
        self,
        descr: TokenDescription,
    ) -> Result<DynamicState, Vec<TokenDescription>> {
        self.state
            .trans(descr, self.stack_top())
            .map(|transition| self.with(transition))
    }

    pub(crate) fn is_accepting(&self) -> bool {
        self.stack.is_empty() && self.state.is_accepting()
    }

    pub(crate) fn fresh_stack(&mut self) -> DynamicState {
        let symbol = self.stack.pop().unwrap();
        DynamicState {
            state: self.state,
            stack: smallvec![symbol],
        }
    }

    pub(crate) fn with_old_stack(&self, old_state: &DynamicState) -> DynamicState {
        assert!(self.stack.is_empty());
        let stack = old_state.stack.clone();

        DynamicState {
            state: self.state,
            stack,
        }
    }

    fn stack_top(&self) -> Option<StackSymbol> {
        self.stack.last().copied()
    }

    fn with(mut self, trans: Transition) -> DynamicState {
        if trans.pop {
            self.stack.pop().unwrap();
        }

        if let Some(symbol) = trans.push {
            self.stack.push(symbol);
        }

        self.state = trans.state;

        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Transition {
    pub(crate) state: State,
    pub(crate) pop: bool,
    pub(crate) push: Option<StackSymbol>,
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
    };
}

macro_rules! generate_grammar {
    (@descr "ident") => {
        TokenDescription::Fragment(FragmentKind::Ident)
    };

    (@descr "expr") => {
        TokenDescription::Fragment(FragmentKind::Expr)
    };

    (@descr $descr:tt) => {
        TokenDescription::$descr
    };

    (@symbol) => {
        None
    };

    (@symbol $symbol:tt) => {
        Some(StackSymbol::$symbol)
    };

    (@state $state:tt) => {
        State::$state
    };

    (@accepting accepting) => {
        true
    };
    (@accepting) => {
        false
    };

    (@inherit $state:ident) => {
        Some(State::$state)
    };
    (@inherit) => {
        None
    };

    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $(
                $( #[$accepting:ident] )?
                $in_state:ident $( ( $inherit:ident ) )? {
                    $(
                        $descr:tt $(, $in_symbol:tt )? => $out_state:tt $(, $out_symbol:tt )?
                    );* $(;)?
                }
            ),* $(,)?
        }
    ) => {
        $( #[$meta] )*
        $vis enum $name {
            $(
                $in_state
            ),*
        }

        impl $name {
            const TRANSITIONS: &'static[
                (
                    &'static [(TokenDescription, Option<StackSymbol>, State, Option<StackSymbol>)],
                    Option<State>,
                )
            ] = &[
                $(
                    (
                        &[
                            $(
                                (
                                    generate_grammar!(@descr $descr),
                                    generate_grammar!(@symbol $( $in_symbol )?),
                                    generate_grammar!(@state $out_state),
                                    generate_grammar!(@symbol $( $out_symbol )?),
                                )
                            ),*
                        ],
                        generate_grammar!(@inherit $( $inherit )?),
                    )
                ),*
            ];

            const ACCEPTING_STATES: &'static [bool] = &[
                $(
                    generate_grammar!(@accepting $( $accepting )?),
                )*
            ];

            pub(crate) fn trans(self, descr: TokenDescription, top: Option<StackSymbol>) -> Result<Transition, Vec<TokenDescription>> {
                let mut state = Some(self);
                let mut errs = Vec::new();

                while let Some(state_) = state {
                    let out_state = Self::TRANSITIONS[state_ as usize].0.iter().find_map(|(descr_, in_sym, out_state, out_sym)| {
                        if descr_ == &descr && (in_sym.is_none() || in_sym == &top) {
                            Some(Transition {
                                state: *out_state,
                                pop: in_sym.is_some(),
                                push: *out_sym,
                            })
                        } else { None }
                    });

                    match out_state {
                        Some(out_state) => return Ok(out_state),
                        None => errs.extend(state_.follow(top)),
                    }

                    state = Self::TRANSITIONS[state_ as usize].1;
                }

                Err(errs)
            }

            fn follow(self, top: Option<StackSymbol>) -> Vec<TokenDescription> {
                Self::TRANSITIONS[self as usize].0.iter().filter_map(|(descr, in_sym, _, _)| {
                    // We try to be smårt here and only suggest tokens that we
                    // can actually accept.
                    if !matches!(descr, TokenDescription::Fragment(_)) && (in_sym.is_none() || in_sym == &top) {
                        Some(*descr)
                    } else {
                        None
                    }
                }).collect()
            }

            fn is_accepting(self) -> bool {
                Self::ACCEPTING_STATES[self as usize]
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

        /// An unused keyword.
        Terminal::Abstract
        | Terminal::Become
        | Terminal::Box
        | Terminal::Do
        | Terminal::Final
        | Terminal::Macro
        | Terminal::Override
        | Terminal::Priv
        | Terminal::Try
        | Terminal::Typeof
        | Terminal::Unsized
        | Terminal::Virtual => UnusedKeyword,

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

generate_grammar! {
    #[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
    pub(crate) enum State {
        ExprStart {
            "ident" => AfterExpr;
            "expr" => AfterExpr;
            Ident => AfterExpr;
            Literal => AfterExpr;
            If => ExprStart, Condition;

            // Array expression
            // https://spec.ferrocene.dev/expressions.html#array-expressions
            LBracket => ExprStart, ArrayExprFirst;

            // <expr> ()
            RParen, FnArgListFirst => AfterExpr;
            // <expr> ( <expr>, )
            RParen, FnArgListThen => AfterExpr;
            // []
            RBracket, ArrayExprFirst => AfterExpr;
            // [ <expr>, ]
            RBracket, ArrayExprThen => AfterExpr;
        },

        #[accepting]
        // Transitions added here must be added in `AfterIf` as well.
        AfterExpr {
            // Arithmetic expressions
            // https://spec.ferrocene.dev/expressions.html#arithmetic-expressions
            Plus => ExprStart;
            Minus => ExprStart;
            Star => ExprStart;
            Slash => ExprStart;
            Percent => ExprStart;

            // Bit expressions
            // https://spec.ferrocene.dev/expressions.html#bit-expressions
            And => ExprStart;
            Or => ExprStart;
            Caret => ExprStart;
            Shl => ExprStart;
            Shr => ExprStart;

            // Comparison expressions
            // https://spec.ferrocene.dev/expressions.html#comparison-expressions
            EqualsEquals => ExprStart;
            GreaterThan => ExprStart;
            GreaterThanEquals => ExprStart;
            LessThan => ExprStart;
            LessThanEquals => ExprStart;
            NotEquals => ExprStart;

            // [ <expr> ]
            RBracket, ArrayExprFirst => AfterExpr;
            RBracket, ArrayExprThen => AfterExpr;

            // [ <expr> ; <expr> ]
            Semicolon, ArrayExprFirst => ExprStart, ArrayExprSize;
            RBracket, ArrayExprSize => AfterExpr;

            RBrace, FnBlockExpr => ItemStart;
            LBrace, Condition => ExprStart, Consequence;

            // <expr> (
            LParen => ExprStart, FnArgListFirst;
            // <expr>, <expr>, ...
            Comma, FnArgListFirst => ExprStart, FnArgListThen;
            Comma, FnArgListThen => ExprStart, FnArgListThen;
            Comma, ArrayExprFirst => ExprStart, ArrayExprThen;
            Comma, ArrayExprThen => ExprStart, ArrayExprThen;
            // <expr> )
            RParen, FnArgListFirst => AfterExpr;
            RParen, FnArgListThen => AfterExpr;

            // We don't continue to `AfterExpr` because we want to parse an
            // optional `else` branch.
            RBrace, Consequence => AfterIf;
            RBrace, Alternative => AfterExpr;

            // <expr> .
            Dot => ExprDot;
        },

        #[accepting]
        AfterIf(AfterExpr) {
            Else => AfterElse;
        },

        // <expr> .
        ExprDot {
            // <expr> . await
            Await => AfterExpr;
            // <expr> . <ident>
            "ident" => FieldOrMethod;
            Ident => FieldOrMethod;

            // <expr> . decimal
            // TODO: this is badness 10_000: this means that we accept things
            // like `"foo"."bar"`, which is obviously not valid Rust.
            //
            // TODO: when the `literal` fragment is recognized, we may want to
            // accept a $literal here as well.
            Literal => AfterExpr;
        },

        // <expr> . <ident>
        #[accepting]
        FieldOrMethod(AfterExpr) {
            // Method call
            // <expr> . <ident> (
            LParen => ExprStart, FnArgListFirst;

            // Turbofish:
            // <expr> . <ident> ::
            ColonColon => CallGenericArgumentList;
        },

        // <expr> . <ident> ::
        CallGenericArgumentList {
            // <expr> . <ident> :: <
            LessThan => GenericStart, CallGenerics;
        },

        // <expr> . <ident> :: <
        GenericStart(TypeStart) {
            // <expr> . <ident> :: < >
            GreaterThan, CallGenerics => AfterMethodCallGenericParams;
        },

        // <expr> . <ident> :: < >
        AfterMethodCallGenericParams {
            // <expr> . <ident> :: < > (
            LParen => ExprStart, FnArgListFirst;
        },

        AfterElse {
            LBrace => ExprStart, Alternative;
        },

        #[accepting]
        ItemStart {
            Fn => AfterFnKw;
        },

        AfterFnKw {
            "ident" => AfterFnName;
            Ident => AfterFnName;
        },

        AfterFnName {
            LParen => FnArgStart, FnParam;
        },

        AfterFnParams {
            RightArrow => TypeStart, AfterFnParams;
            LBrace => ExprStart, FnBlockExpr;
        },

        FnArgStart {
            "ident" => AfterFnParamName;
            Ident => AfterFnParamName;
            RParen, FnParam => AfterFnParams
        },

        AfterFnParamName {
            Colon => TypeStart;
        },

        TypeStart {
            "ident" => AfterType;
            Ident => AfterType;
        },

        AfterType {
            Comma, FnParam => FnArgStart, FnParam;
            RParen, FnParam => AfterFnParams;
            LBrace, AfterFnParams => ExprStart, FnBlockExpr;

            // fn_name :: < <type> ,
            Comma, CallGenerics => GenericStart, CallGenerics;
            // fn_name :: < <type> >
            GreaterThan, CallGenerics => AfterMethodCallGenericParams;
        },
    }
}

impl State {
    pub(crate) fn into_dynamic_state(self) -> DynamicState {
        DynamicState {
            state: self,
            stack: smallvec![],
        }
    }
}

// We probably want more, more descriptive, names for these.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum StackSymbol {
    FnBlockExpr,
    Condition,
    Consequence,
    Alternative,
    FnArgListFirst,
    FnArgListThen,
    FnParam,
    AfterFnParams,
    ArrayExprFirst,
    ArrayExprThen,
    ArrayExprSize,
    CallGenerics,
}
