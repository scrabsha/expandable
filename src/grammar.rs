use tinyset::Fits64;

use crate::{FragmentKind, Terminal};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct DynamicState {
    pub(crate) state: State,
    // This is probably the most disappointing part of the codebase.
    //
    // The Rust grammar is definitely not regular. It should be parsed with
    // pushdown automaton. We circumvent this issue by manipulating trees
    // instead of raw sequence of tokens. This approach works well, as long as
    // we can guess in advance if a token is a delimiter or not.
    //
    // Depending on the situation, `<` and `>` may or may not be considered as
    // delimiters. For instance, `<` and `>` are delimiters in
    // `Iterator<Item = u8>`, but are definitely not delimiters in
    // `4 < x && 6 > y`. As a result, we do have to use a pushdown automaton in
    // order to parse the Rust syntax.
    //
    // Luckily for us, there is only one symbol that is pushed and popped in the
    // state machine's stack, which represent the amount of "delimiter `<`" that
    // have not been closed by a "delimiter `>`". This means that this entire
    // stack can be represented with an integer.
    pub(crate) opened_lts: u8,
}

type ExpectedTerminals = &'static [TokenDescription];

impl DynamicState {
    pub(crate) fn accept_fragment(
        self,
        fragment: FragmentKind,
    ) -> Result<DynamicState, ExpectedTerminals> {
        let new_state = self.state.accept_fragment(fragment)?;
        Ok(self.with_state_and_delta(new_state, Delta::Zero))
    }
    pub(crate) fn accept_terminal(
        self,
        terminal: &Terminal,
    ) -> Result<DynamicState, ExpectedTerminals> {
        let (new_state, delta) = self.state.accept_terminal(terminal)?;
        Ok(self.with_state_and_delta(new_state, delta))
    }

    pub(crate) fn accept_paren(self) -> Result<(DynamicState, DynamicState), ExpectedTerminals> {
        let (inner, next) = self.state.accept_paren()?;
        Ok((
            self.with_state_and_delta(inner, Delta::Zero),
            self.with_state_and_delta(next, Delta::Zero),
        ))
    }

    pub(crate) fn accept_curly(self) -> Result<(DynamicState, DynamicState), ExpectedTerminals> {
        let (inner, next) = self.state.accept_curly()?;
        Ok((
            self.with_state_and_delta(inner, Delta::Zero),
            self.with_state_and_delta(next, Delta::Zero),
        ))
    }

    #[inline]
    const fn with_state_and_delta(self, state: State, delta: Delta) -> DynamicState {
        let opened_lts = self.opened_lts + delta as u8;
        DynamicState { state, opened_lts }
    }

    pub(crate) fn is_accepting(self) -> bool {
        self.state.is_accepting() && self.opened_lts == 0
    }

    pub(crate) const fn increment_to(self, state: State) -> DynamicState {
        let opened_lts = self.opened_lts + 1;

        DynamicState { state, opened_lts }
    }

    pub(crate) const fn decrement_to(self, state: State) -> Option<DynamicState> {
        // HACK: once `?` is legal in const context, replace this `match` with
        // a `?` thing.
        let opened_lts = match self.opened_lts.checked_sub(1) {
            Some(v) => v,
            None => return None,
        };

        Some(DynamicState { state, opened_lts })
    }
}

impl Fits64 for DynamicState {
    unsafe fn from_u64(x: u64) -> DynamicState {
        let [a, b, c, ..] = u64::to_ne_bytes(x);

        DynamicState {
            opened_lts: a,
            state: State::from_u16(u16::from_ne_bytes([b, c])),
        }
    }

    fn to_u64(self) -> u64 {
        let a = self.opened_lts;
        let [b, c] = u16::to_ne_bytes(self.state.to_u16());
        u64::from_ne_bytes([a, b, c, 0, 0, 0, 0, 0])
    }
}

macro_rules! token_description {
    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $(
                $pattern:pat => $variant:ident
            ),* $(,)?
        }
    ) => {
        $( #[$meta] )*
        $vis enum $name {
            Paren,
            Square,
            Bracket,
            Invalid,

            $( $variant ),*
        }

        impl $name {
            $vis fn matches(self, terminal: &$crate::Terminal) -> bool {
                use $name::*;

                match self {
                    Paren | Square | Bracket => false,
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
        TokenDescription::Square
    };
    (@mk_descr {}) => {
        TokenDescription::Bracket
    };
    (@mk_descr $ident:ident) => {
        TokenDescription::$ident
    };
    (@mk_descr $lit:literal) => {
        TokenDescription::Invalid
    };

    (@terminal ( $inner:ident ), $out_state:ident ) => {
        $out_state
    };
    (@terminal [ $inner:ident ], $out_state:ident ) => {
        $out_state
    };
    (@terminal { $inner:ident }, $out_state:ident ) => {
        $out_state
    };
    (@terminal $ident:ident) => {
        $ident
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
    (@inner $any:tt) => {
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
                        $descr:tt => $out_state:tt $(, $out_state_2:ident )?
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
            $vis fn accept_fragment(&self, kind: FragmentKind) -> Result<$name, ExpectedTerminals> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@as_fragment kind $descr) {
                                    #[allow(unreachable_code, clippy::diverging_sub_expression)]
                                    return Ok(generate_grammar!(@terminal $out_state $(, $out_state_2)?));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression)]
            $vis fn accept_paren(&self) -> Result<($name, $name), ExpectedTerminals> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@same_delim $descr ()) {
                                    #[allow(unreachable_code)]
                                    return Ok(generate_grammar!(@inner $out_state $(, $out_state_2)?));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression, unused)]
            $vis fn accept_square(&self) -> Result<($name, $name), ExpectedTerminals> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@same_delim $descr []) {
                                    #[allow(unreachable_code, )]
                                    #[allow(clippy::diverging_sub_expression)]
                                    return Ok(generate_grammar!(@inner $out_state $(, $out_state_2)?));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            #[allow(clippy::diverging_sub_expression)]
            $vis fn accept_curly(&self) -> Result<($name, $name), ExpectedTerminals> {
                use $name::*;
                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@same_delim $descr {}) {
                                    #[allow(unreachable_code)]
                                    #[allow(clippy::diverging_sub_expression)]
                                    return Ok(generate_grammar!(@inner $out_state $(, $out_state_2)?));
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
            ) -> Result<($name, Delta), ExpectedTerminals> {
                use $name::*;

                match self {
                    $(
                        $in_state => {
                            $(
                                if generate_grammar!(@mk_descr $descr).matches(terminal) {
                                    #[allow(clippy::diverging_sub_expression)]
                                    return Ok((generate_grammar!(@terminal $out_state $(, $out_state_2)?), Delta::Zero));
                                }
                            )*
                            return Err(self.follow());
                        }
                    ),*
                }
            }

            fn follow(&self) -> ExpectedTerminals {
                use $name::*;

                match self {
                    $(
                        $in_state => &[
                            $(
                                generate_grammar!(@mk_descr $descr)
                            ),*
                        ]
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
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum TokenDescription {
        Terminal::Ident(_) => Ident,
        Terminal::Fn => Fn,
        Terminal::Plus => Plus,
        Terminal::Times => Times,
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
        FnParamStart {}
    }
}

impl State {
    pub(crate) fn into_dynamic_state(self) -> DynamicState {
        DynamicState {
            state: self,
            opened_lts: 0,
        }
    }
}

pub(crate) enum Delta {
    MinusOne = -1,
    Zero = 0,
    PlusOne = 1,
}
