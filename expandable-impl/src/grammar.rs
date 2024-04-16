// Architectural invariant: this module contains basic types that allow to parse
// the Rust language.

use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr,
};

use smallvec::SmallVec;

use crate::{FragmentKind, Terminal};

#[derive(Clone, Debug)]
pub(crate) struct DynamicState<Span> {
    pub(crate) state: State,
    pub(crate) stack: SmallVec<[StackSymbol; 16]>,
    inner: PhantomData<Span>,
}

impl<Span> PartialEq for DynamicState<Span>
where
    Span: 'static,
{
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other) || self.state == other.state
    }
}

impl<Span> Eq for DynamicState<Span> where Span: 'static {}

impl<Span> Hash for DynamicState<Span> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.state.hash(state);
    }
}

impl<Span> DynamicState<Span> {
    pub(crate) fn expr() -> DynamicState<Span> {
        DynamicState {
            state: State::ExprStart,
            stack: SmallVec::new(),
            inner: PhantomData,
        }
    }

    pub(crate) fn item() -> DynamicState<Span> {
        DynamicState {
            state: State::ItemStart,
            stack: SmallVec::new(),
            inner: PhantomData,
        }
    }

    pub(crate) fn accept_fragment(
        self,
        fragment: FragmentKind,
        s: Span,
    ) -> Result<(DynamicState<Span>, Transition), (Span, Vec<TokenDescription>)> {
        self.accept(TokenDescription::Fragment(fragment), s)
    }

    pub(crate) fn accept(
        self,
        descr: TokenDescription,
        s: Span,
    ) -> Result<(DynamicState<Span>, Transition), (Span, Vec<TokenDescription>)> {
        self.state
            .trans(descr, self.stack_top())
            .map(|transition| {
                (
                    self.with(transition.clone()),
                    Transition::from_raw(transition),
                )
            })
            .map_err(|e| (s, e))
    }

    pub(crate) fn is_accepting(&self) -> Result<(), Option<(Span, Vec<TokenDescription>)>> {
        if self.stack.is_empty() && self.state.is_accepting() {
            Ok(())
        } else {
            Err(None)
        }
    }

    fn stack_top(&self) -> Option<StackSymbol> {
        self.stack.last().copied()
    }

    fn with(mut self, trans: GrammarTransition) -> DynamicState<Span> {
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct Transition {
    popped: usize,
    pushed: Vec<String>,
}

impl Transition {
    fn from_raw(trans: GrammarTransition) -> Transition {
        Transition {
            popped: if trans.pop { 1 } else { 0 },
            pushed: trans.push.into_iter().map(|s| format!("{s:?}")).collect(),
        }
    }

    pub(crate) fn empty() -> Transition {
        Transition {
            popped: 0,
            pushed: vec![],
        }
    }

    pub(crate) fn combine_chasles(mut self, other: Transition) -> Transition {
        for _ in 0..other.popped {
            self.log_pop();
        }
        for pushed in other.pushed {
            self.log_push(pushed);
        }
        self
    }

    fn log_pop(&mut self) {
        if self.pushed.pop().is_none() {
            self.popped += 1;
        }
    }

    fn log_push(&mut self, state: String) {
        self.pushed.push(state);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct GrammarTransition {
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

            pub(crate) fn trans(self, descr: TokenDescription, top: Option<StackSymbol>) -> Result<GrammarTransition, Vec<TokenDescription>> {
                let mut state = Some(self);
                let mut errs = Vec::new();

                while let Some(state_) = state {
                    let out_state = Self::TRANSITIONS[state_ as usize].0.iter().find_map(|(descr_, in_sym, out_state, out_sym)| {
                        if descr_ == &descr && (in_sym.is_none() || in_sym == &top) {
                            Some(GrammarTransition {
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
            "ident" => AfterIdentExpr;
            "expr" => AfterExpr;
            Ident => AfterIdentExpr;
            Literal => AfterExpr;
            If => ExprStart, Condition;

            // Array expression
            // https://spec.ferrocene.dev/expressions.html#array-expressions
            LBracket => ExprStart, ArrayExprFirst;

            // Block expressions
            // https://spec.ferrocene.dev/expressions.html#syntax_blockexpression
            LBrace => StmtStart, BlockExpr;

            // <expr> ( <expr> ,)
            RParen, FnArgList => AfterExpr;
            // []
            RBracket, ArrayExprFirst => AfterExpr;
            // [ <expr>, ]
            RBracket, ArrayExprThen => AfterExpr;

            // break <expr>
            // TODO: handle labels
            Break => AfterBreakOrReturn;
            // return <expr>
            Return => AfterBreakOrReturn;
        },

        // <ident>
        #[accepting]
        AfterIdentExpr(AfterExpr) {
            // <ident> ::
            ColonColon => CallGenericArgumentList;
        },

        // break <expr>
        // return <expr>
        #[accepting]
        AfterBreakOrReturn(ExprStart) {},

        #[accepting]
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
            AndAnd => ExprStart;
            OrOr => ExprStart;

            // Comparison expressions
            // https://spec.ferrocene.dev/expressions.html#comparison-expressions
            EqualsEquals => ExprStart;
            GreaterThan => ExprStart;
            GreaterThanEquals => ExprStart;
            LessThan => ExprStart;
            LessThanEquals => ExprStart;
            NotEquals => ExprStart;

            // Range expressions
            DotDot => ExprStart;
            DotDotEquals => ExprStart;

            // [ <expr> ]
            RBracket, ArrayExprFirst => AfterExpr;
            RBracket, ArrayExprThen => AfterExpr;

            // [ <expr> ; <expr> ]
            Semicolon, ArrayExprFirst => ExprStart, ArrayExprSize;
            RBracket, ArrayExprSize => AfterExpr;

            RBrace, FnBlockExpr => ItemStart;
            LBrace, Condition => ExprStart, Consequence;

            // <expr> (
            LParen => ExprStart, FnArgList;
            // <expr>, <expr>, ...
            Comma, FnArgList => ExprStart, FnArgList;
            Comma, ArrayExprFirst => ExprStart, ArrayExprThen;
            Comma, ArrayExprThen => ExprStart, ArrayExprThen;
            // <expr> )
            RParen, FnArgList => AfterExpr;

            // We don't continue to `AfterExpr` because we want to parse an
            // optional `else` branch.
            RBrace, Consequence => AfterIf;
            RBrace, Alternative => AfterExpr;

            // { <expr> }
            RBrace, BlockExpr => AfterExpr;
            RBrace, GenericBlockExpr => AfterGenericExpr;

            // <expr> .
            Dot => ExprDot;

            // <expr> ;
            Semicolon, BlockExpr => StmtStart, BlockExpr;
            Semicolon, FnBlockExpr => StmtStart, FnBlockExpr;
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
            LParen => ExprStart, FnArgList;

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
            GreaterThan, CallGenerics => AfterCallGenericParams;
            // <expr> . <ident> :: < { <expr> } >
            LBrace => ExprStart, GenericBlockExpr;
            // <expr> . <ident> :: < literal >
            Literal => AfterGenericExpr;
            // <expr> . <ident> :: < - <literal> >
            Minus => GenericLiteralExpr;
        },

        // <expr> . <ident> :: < - <literal> >
        GenericLiteralExpr {
            Literal => AfterGenericExpr;
        },

        // <expr> . <ident> :: < { <expr> }
        AfterGenericExpr {
            Comma => GenericStart;
            GreaterThan, CallGenerics => AfterCallGenericParams;
        },

        // <expr> . <ident> :: < >
        AfterCallGenericParams {
            // <expr> . <ident> :: < > (
            LParen => ExprStart, FnArgList;
        },

        AfterElse {
            LBrace => ExprStart, Alternative;
        },

        #[accepting]
        StmtStart(ExprStart) {
            // Let statement
            Let => PatternStart, LetStmt;
        },

        PatternStart {
            // let <pattern>
            "ident" => AfterPattern;
            Ident => AfterPattern;
        },

        AfterPattern {
            // TODO: type ascription.
            // let <pattern> =
            Equals, LetStmt => ExprStart;
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
            LBrace => StmtStart, FnBlockExpr;
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
            LBrace, AfterFnParams => StmtStart, FnBlockExpr;

            // fn_name :: < <type> ,
            Comma, CallGenerics => GenericStart, CallGenerics;
            // fn_name :: < <type> >
            GreaterThan, CallGenerics => AfterCallGenericParams;
        },
    }
}

// We probably want more, more descriptive, names for these.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum StackSymbol {
    FnBlockExpr,
    Condition,
    Consequence,
    Alternative,
    FnArgList,
    FnParam,
    AfterFnParams,
    // We have to distinguish between the first and the other elements of an
    // array in order to detect ArrayRepetitionConstructors:
    //
    // https://spec.ferrocene.dev/expressions.html#syntax_arrayrepetitionconstructor
    ArrayExprFirst,
    ArrayExprThen,
    ArrayExprSize,
    CallGenerics,
    BlockExpr,
    GenericBlockExpr,
    LetStmt,
}
