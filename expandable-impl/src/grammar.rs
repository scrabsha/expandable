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
        self.trans(TokenDescription::Fragment(fragment))
    }

    pub(crate) fn accept<Descr>(self, descr: Descr) -> Result<DynamicState, Vec<TokenDescription>>
    where
        Descr: Into<TokenDescription>,
    {
        self.trans(descr.into())
    }

    pub(crate) fn is_accepting(&self) -> bool {
        self.stack.is_empty() && self.state.is_accepting()
    }

    pub(crate) fn fresh_stack(&self) -> DynamicState {
        let symbol = self.stack.last().unwrap();
        DynamicState {
            state: self.state,
            stack: smallvec![*symbol],
        }
    }

    pub(crate) fn with_old_stack(&self, old_state: &DynamicState) -> DynamicState {
        DynamicState {
            state: self.state,
            stack: old_state.stack.clone(),
        }
    }

    fn trans(self, descr: TokenDescription) -> Result<DynamicState, Vec<TokenDescription>> {
        self.state
            .trans(descr, self.stack_top())
            .map(|transition| self.with(transition))
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

            // `FragmentKind` is a private enum, and this enum is exposed to the
            // end user, triggering the `private_interfaces` warning.
            //
            // `$name` has to stay public for error reporting reasons, but it
            // feels incorrect to mark `FragmentKind` as public as well. Adding
            // a public-only type that does not have the `FragmentKind` variant
            // seems like a waste of time and a slightly too overengineered
            // solution.
            #[doc(hidden)]
            #[allow(private_interfaces)]
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

    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $(
                $( #[$accepting:ident] )?
                $in_state:ident {
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
                &'static [(TokenDescription, Option<StackSymbol>, State, Option<StackSymbol>)]
            ] = &[
                $(
                    &[
                        $(
                            (
                                generate_grammar!(@descr $descr),
                                generate_grammar!(@symbol $( $in_symbol )?),
                                generate_grammar!(@state $out_state),
                                generate_grammar!(@symbol $( $out_symbol )?),
                            )
                        ),*
                    ]
                ),*
            ];

            const ACCEPTING_STATES: &'static [bool] = &[
                $(
                    generate_grammar!(@accepting $( $accepting )?),
                )*
            ];

            pub(crate) fn trans(self, descr: TokenDescription, top: Option<StackSymbol>) -> Result<Transition, Vec<TokenDescription>> {
                Self::TRANSITIONS[self as usize].iter().find_map(|(descr_, in_sym, out_state, out_sym)| {
                    if descr_ == &descr && (in_sym.is_none() || in_sym == &top) {
                        Some(Transition {
                            state: *out_state,
                            pop: in_sym.is_some(),
                            push: *out_sym,
                        })
                    } else { None }
                }).ok_or_else(|| self.follow())
            }

            fn follow(self) -> Vec<TokenDescription> {
               Self::TRANSITIONS[self as usize].iter().filter_map(|(descr, _, _, _)| {
                    if matches!(descr, TokenDescription::Fragment(_)) {
                        None
                    } else {
                        Some(*descr)
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
    ///
    /// [`Terminal`]: Terminal
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum TokenDescription {
        /// An identifier. Does not include keywords.
        Terminal::Ident(_) => Ident,
        /// The `fn` token.
        Terminal::Fn => Fn,
        /// A plus (`+`).
        Terminal::Plus => Plus,
        /// A times (`*`).
        Terminal::Times => Times,
        /// A colon (`:`).
        Terminal::Colon => Colon,
        /// A comma (`,`).
        Terminal::Comma => Comma,
        /// An arrow (`->`).
        Terminal::Arrow => Arrow,
        /// A fat arrow (`=>`).
        Terminal::FatArrow => FatArrow,
        /// A semicolon (`;`).
        Terminal::Semi => Semi,
        /// A question mark (`?`).
        Terminal::QuestionMark => QuestionMark,
        /// The dollar sign (`$`).
        Terminal::Dollar => Dollar,
    }
}

generate_grammar! {
    #[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
    pub(crate) enum State {
        ExprStart {
            "ident" => AfterExpr;
            "expr" => AfterExpr;
            Ident => AfterExpr;
        },

        #[accepting]
        AfterExpr {
            Plus => ExprStart;
            Times => ExprStart;
            RBrace, FnBlockExpr => ItemStart;
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
            LParen => FnParamStart, FnParamList
        },

        AfterFnParams {
            LBrace => ExprStart, FnBlockExpr;
        },

        FnParamStart {
            "ident" => AfterFnParamName;
            Ident => AfterFnParamName;
            RParen, FnParamList => AfterFnParams
        },

        AfterFnParamName {
            Colon => TypeStart;
        },

        TypeStart {
            "ident" => AfterType;
            Ident => AfterType;
        },

        AfterType {
            Comma, FnParamList => FnParamStart, FnParamList;
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
    FnParamList,
    FnBlockExpr,
}
