use smallvec::{smallvec, SmallVec};

use crate::{FragmentKind, Terminal};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct DynamicState {
    pub(crate) states: SmallVec<[State; 8]>,
}

impl DynamicState {
    pub(crate) fn accept_fragment(
        self,
        fragment: FragmentKind,
    ) -> Result<DynamicState, Vec<TokenDescription>> {
        self.top()
            .accept_fragment(fragment)
            .map(|trans| self.with(trans))
    }

    pub(crate) fn accept_terminal(
        self,
        terminal: &Terminal,
    ) -> Result<DynamicState, Vec<TokenDescription>> {
        self.top()
            .accept_terminal(terminal)
            .map(|trans| self.with(trans))
    }

    pub(crate) fn accept_paren(
        self,
    ) -> Result<(DynamicState, DynamicState), Vec<TokenDescription>> {
        let (inner, next) = self.top().accept_paren()?;
        Ok((inner.into_dynamic_state(), self.with_top_state(next)))
    }

    pub(crate) fn accept_curly(
        self,
    ) -> Result<(DynamicState, DynamicState), Vec<TokenDescription>> {
        let (inner, next) = self.top().accept_brace()?;
        Ok((inner.into_dynamic_state(), self.with_top_state(next)))
    }

    pub(crate) fn is_accepting(&self) -> bool {
        self.states.len() == 1 && self.top().is_accepting()
    }

    fn with(mut self, trans: FragmentTransition) -> DynamicState {
        match trans {
            FragmentTransition::Nest { inner, next } => {
                *self.top_mut() = next;
                self.states.push(inner);
            }
            FragmentTransition::Flat { next } => {
                *self.top_mut() = next;
            }
            FragmentTransition::Finish => {
                self.states.pop().unwrap();
            }
        }

        self
    }

    #[inline]
    fn top(&self) -> State {
        *self.states.last().unwrap()
    }

    #[inline]
    fn top_mut(&mut self) -> &mut State {
        self.states.last_mut().unwrap()
    }

    #[inline]
    fn with_top_state(mut self, state: State) -> DynamicState {
        *self.top_mut() = state;
        self
    }
}

pub(crate) enum FragmentTransition {
    // The terminal we just accepted is a delimiter. Advance to a new state
    // and push the old state to the stack.
    Nest { inner: State, next: State },
    // The terminal we just accepted is not a delimiter.
    Flat { next: State },
    // The terminal we just accepted is a closing delimiter. Pop the stack and
    // continue.
    Finish,
}

impl From<[State; 2]> for FragmentTransition {
    fn from([inner, next]: [State; 2]) -> Self {
        FragmentTransition::Nest { inner, next }
    }
}

impl From<[State; 1]> for FragmentTransition {
    fn from([next]: [State; 1]) -> Self {
        FragmentTransition::Flat { next }
    }
}

impl From<[State; 0]> for FragmentTransition {
    fn from(_: [State; 0]) -> Self {
        FragmentTransition::Finish
    }
}

impl From<[State; 1]> for State {
    fn from([state]: [State; 1]) -> Self {
        state
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
        $vis enum $name {
            /// An opening or closing parenthesis.
            Paren,
            /// An opening or closing bracket.
            Bracket,
            /// An opening or closing brace.
            Brace,
            /// An invalid token.
            Invalid,

            $(
                $( #[$meta_] )*
                $variant
            ),*
        }

        impl $name {
            pub(crate) fn matches(self, terminal: &$crate::Terminal) -> bool {
                use $name::*;

                match self {
                    Paren | Brace | Bracket => false,
                    Invalid => false,

                    $( $name::$variant => matches!(terminal, $pattern) ),*
                }
            }
        }
    };
}

macro_rules! generate_grammar {
    (@accepting accepting) => { true };
    (@accepting) => { false };

    (@mk_descr ()) => {
        TokenDescription::Paren
    };
    (@mk_descr []) => {
        TokenDescription::Bracket
    };
    (@mk_descr {}) => {
        TokenDescription::Brace
    };
    (@mk_descr $ident:ident) => {
        TokenDescription::$ident
    };
    (@mk_descr $lit:literal) => {
        TokenDescription::Invalid
    };

    (@terminal ( $inner:ident ), $out_state:ident ) => {
        [ $out_state ].into()
    };
    (@terminal [ $inner:ident ], $out_state:ident ) => {
        [ $out_state ].into()
    };
    (@terminal { $inner:ident }, $out_state:ident ) => {
        [ $out_state ].into()
    };
    (@terminal _) => {
        FragmentTransition::Finish
    };
    (@terminal $( $ident:ident ),*) => {
        [ $( $ident ),* ].into()
    };

    (@inner ( $inner:ident ), $final:ident) => {
        ($inner, $final)
    };
    (@inner [ $inner:ident ], $final:ident) => {
        ($inner, $final)
    };
    (@inner { $inner:ident }, $final:ident) => {
        ($inner, $final)
    };
    (@inner $( $tail:tt )* ) => {
        panic!("Bad logic")
    };

    (@same_delim () ()) => { true };
    (@same_delim [] []) => { true };
    (@same_delim {} {}) => { true };
    (@same_delim $any:tt $any_:tt) => { false };

    (@as_fragment $kind:ident "ident") => {
        $kind == FragmentKind::Ident
    };
    (@as_fragment $kind:ident "expr") => {
        $kind == FragmentKind::Expr
    };
    (@as_fragment $a:tt $b:tt) => {
        false
    };

    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $(
                $( #[$accepting:ident] )?
                $in_state:ident {
                    $(
                        $descr:tt => $( $out_state:tt ),*
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
            #[allow(clippy::diverging_sub_expression)]
            pub(crate) fn accept_fragment(&self, kind: FragmentKind) -> Result<FragmentTransition, Vec<TokenDescription>> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@as_fragment kind $descr) {
                                    #[allow(unreachable_code, clippy::diverging_sub_expression)]
                                    return Ok(generate_grammar!(@terminal $( $out_state ),*));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression)]
            pub(crate) fn accept_paren(&self) -> Result<($name, $name), Vec<TokenDescription>> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@same_delim $descr ()) {
                                    #[allow(unreachable_code)]
                                    return Ok(generate_grammar!(@inner $( $out_state ),* ));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression, unused)]
            pub(crate) fn accept_bracket(&self) -> Result<($name, $name), Vec<TokenDescription>> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@same_delim $descr []) {
                                    #[allow(unreachable_code, )]
                                    #[allow(clippy::diverging_sub_expression)]
                                    return Ok(generate_grammar!(@inner $( $out_state ),*));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression)]
            pub(crate) fn accept_brace(&self) -> Result<($name, $name), Vec<TokenDescription>> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@same_delim $descr {}) {
                                    #[allow(unreachable_code)]
                                    #[allow(clippy::diverging_sub_expression)]
                                    return Ok(generate_grammar!(@inner $( $out_state ),*));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression)]
            pub(crate) fn accept_terminal(
                &self, terminal: &Terminal
            ) -> Result<FragmentTransition, Vec<TokenDescription>> {
                use $name::*;

                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@mk_descr $descr).matches(terminal) {
                                    #[allow(clippy::diverging_sub_expression)]
                                    return Ok((generate_grammar!(@terminal $( $out_state ),*)));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            // A helper for the `follow` method
            fn remove_invalid_descrs(descrs: &[TokenDescription]) -> Vec<TokenDescription> {
                descrs.iter().filter(|descr| **descr != TokenDescription::Invalid).copied().collect()
            }

            fn follow(&self) -> Vec<TokenDescription> {
                use $name::*;

                match self {
                    $(
                        $in_state => $name::remove_invalid_descrs(&[
                            $(
                                generate_grammar!(@mk_descr $descr)
                            ),*
                        ])
                    ),*
                }
            }

            fn is_accepting(&self) -> bool {
                const ACCEPTING_VALUES: &[bool] = &[
                    $(
                        generate_grammar!(@accepting $( $accepting )?)
                    ),*
                ];

                ACCEPTING_VALUES[*self as usize]
            }

            fn from_u16(input: u16) -> $name {
                use $name::*;
                const VALUES: &[$name] = &[ $( $in_state ),* ];

                VALUES[input as usize]
            }

            fn to_u16(self) -> u16 {
                self as u16
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
            () => (FnParamStart), AfterFnParam
        },

        AfterFnParam {
            {} => { ExprStart }, ItemStart
        },

        #[accepting]
        FnParamStart {
            "ident" => AfterFnParamName;
            Ident => AfterFnParamName;
        },

        AfterFnParamName {
            Colon => TypeStart, AfterFnParamType;
        },

        TypeStart {
        },

        AfterFnParamType {
            Comma => FnParamStart
        },
    }
}

impl State {
    pub(crate) fn into_dynamic_state(self) -> DynamicState {
        DynamicState {
            states: smallvec![self],
        }
    }
}
