use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn runtime_base() -> TokenStream {
    quote! {
        #![allow(
            unused,
            non_snake_case,
        )]

        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum TokenDescription {
            Ident,
            As,
            Async,
            Await,
            Break,
            Const,
            Continue,
            Crate,
            Dyn,
            Else,
            Enum,
            Extern,
            False,
            Fn,
            For,
            If,
            Impl,
            In,
            Let,
            Loop,
            Match,
            Mod,
            Move,
            Mut,
            Pub,
            Ref,
            Return,
            Self_,
            SelfUpper,
            Static,
            Struct,
            Super,
            Trait,
            True,
            Type,
            Union,
            Unsafe,
            Use,
            Where,
            While,
            Yield,

            Abstract,
            Become,
            Box,
            Do,
            Final,
            Macro,
            Override,
            Priv,
            Try,
            Typeof,
            Unsized,
            Virtual,

            Literal,
            Plus,
            Minus,
            Star,
            Slash,
            Percent,
            Caret,
            Not,
            And,
            Or,
            AndAnd,
            OrOr,
            Shl,
            Shr,
            PlusEquals,
            MinusEquals,
            StarEquals,
            SlashEquals,
            PercentEquals,
            CaretEquals,
            AndEquals,
            OrEquals,
            ShlEquals,
            ShrEquals,
            Equals,
            EqualsEquals,
            NotEquals,
            GreaterThan,
            LessThan,
            GreaterThanEquals,
            LessThanEquals,
            At,
            Underscore,
            Dot,
            DotDot,
            DotDotDot,
            DotDotEquals,
            Comma,
            Semicolon,
            Colon,
            ColonColon,
            RightArrow,
            FatArrow,
            Pound,
            Dollar,
            QuestionMark,

            LParen,
            RParen,
            LBracket,
            RBracket,
            LBrace,
            RBrace,

            FragmentBlock,
            FragmentExpr,
            FragmentIdent,
            FragmentItem,
            FragmentLifetime,
            FragmentLiteral,
            FragmentMeta,
            FragmentPat,
            FragmentPath,
            FragmentStmt,
            FragmentTT,
            FragmentTy,
            FragmentVis,
        }

        use TokenDescription::*;

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct RustParser {
            buffer: TokenBuffer,
            stack: Vec<State>,
        }

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct TransitionData {
            pub popped: usize,
            pub buf_size: usize,
            // TODO: wrap this in an opaque type.
            pub pushed: Vec<&'static str>,
        }

        impl RustParser {
            pub fn item() -> RustParser {
                RustParser {
                    buffer: TokenBuffer::Empty([]),
                    stack: vec![(item, "<item entry point>")],
                }
            }

            pub fn expr() -> RustParser {
                RustParser {
                    buffer: TokenBuffer::Empty([]),
                    stack: vec![(expr, "<expr entry point>")],
                }
            }

            pub fn step(&mut self, token: TokenDescription) -> Option<TransitionData> {
                match self.buffer.len() {
                    buf_size @ (0 | 1) => {
                        self.buffer.push(token);
                        return Some(TransitionData {
                            popped: 0,
                            buf_size,
                            pushed: vec![],
                        });
                    },

                    2 => {
                        self.buffer.push(token);
                    },

                    _ => panic!("Token buffer is full!"),
                };

                self.perform_progress()
            }

            pub fn finish(&mut self) -> Option<()> {
                while !self.buffer.is_empty() {
                    self.perform_progress()?;
                }

                Some(())
            }

            fn perform_progress(&mut self) -> Option<TransitionData> {
                let mut trans = TransitionData::new_full_buffer();

                let mut fuel = u8::MAX;
                loop {
                    assert!(fuel != 0, "Out of fuel. This is probably a bug in the parser codegen.");
                    fuel -= 1;

                    // Expected eof, found `...`
                    trans.log_pop();
                    let (state, name) = self.stack.pop()?;
                    match state(self)? {
                        Transition::CallNow(states) => {
                            states.iter().rev().for_each(|(_, fn_)| trans.log_push(fn_));
                            self.stack.extend(states.iter().rev().copied());
                        },
                        Transition::CallThen(states) => {
                            self.buffer.shift();
                            states.iter().rev().for_each(|(_, fn_)| trans.log_push(fn_));
                            self.stack.extend(states.iter().rev().copied());
                            break Some(trans);
                        },
                    }
                }
            }
        }

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        enum TokenBuffer {
            Empty([TokenDescription; 0]),
            Single([TokenDescription; 1]),
            Double([TokenDescription; 2]),
            Triple([TokenDescription; 3]),
        }

        type State = (fn(&mut RustParser) -> Option<Transition>, &'static str);

        #[derive(Clone, Copy, Debug, PartialEq)]
        enum Transition {
            // No token has been consumed
            CallNow(&'static [State]),
            // Exactly one token has been consumed
            CallThen(&'static [State]),
        }

        impl TokenBuffer {
            fn len(&self) -> usize {
                match self {
                    TokenBuffer::Empty(_) => 0,
                    TokenBuffer::Single(_) => 1,
                    TokenBuffer::Double(_) => 2,
                    TokenBuffer::Triple(_) => 3,
                }
            }

            fn is_empty(&self) -> bool {
                self.len() == 0
            }

            fn push(&mut self, token: TokenDescription) {
                match self {
                    TokenBuffer::Empty([]) => {
                        *self = TokenBuffer::Single([token]);
                    },
                    TokenBuffer::Single([a]) => {
                        *self = TokenBuffer::Double([*a, token]);
                    },
                    TokenBuffer::Double([a, b]) => {
                        *self = TokenBuffer::Triple([*a, *b, token]);
                    },
                    TokenBuffer::Triple([a, b, c]) => {
                        panic!("Token buffer is full!");
                    },
                }
            }

            fn shift(&mut self) -> TokenDescription {
                match self {
                    TokenBuffer::Empty(_) => panic!("Token buffer is empty!"),
                    TokenBuffer::Single([a]) => {
                        let a = *a;
                        *self = TokenBuffer::Empty([]);
                        a
                    },
                    TokenBuffer::Double([a, b]) => {
                        let a = *a;
                        *self = TokenBuffer::Single([*b]);
                        a
                    },
                    TokenBuffer::Triple([a, b, c]) => {
                        let a = *a;
                        *self = TokenBuffer::Double([*b, *c]);
                        a
                    },
                }
            }

            fn peek(&self) -> TokenDescription {
                match self {
                    TokenBuffer::Empty(_) => panic!("Token buffer is empty!"),
                    TokenBuffer::Single([a])
                    | TokenBuffer::Double([a, _])
                    | TokenBuffer::Triple([a, _, _]) => *a,
                }
            }

            fn peek2(&self) -> TokenDescription {
                match self {
                    TokenBuffer::Empty(_) => panic!("Token buffer is empty!"),
                    TokenBuffer::Single([_]) => panic!("Token buffer has only one element!"),
                    TokenBuffer::Double([_, a])
                    | TokenBuffer::Triple([_, a, _]) => *a,
                }
            }

            fn peek3(&self) -> TokenDescription {
                match self {
                    TokenBuffer::Empty(_) => panic!("Token buffer is empty!"),
                    TokenBuffer::Single([_]) => panic!("Token buffer has only one element!"),
                    TokenBuffer::Double([_, _]) => panic!("Token buffer has only two elements!"),
                    TokenBuffer::Triple([_, _, a]) => *a,
                }
            }
        }

        impl TransitionData {
            fn new_full_buffer() -> TransitionData {
                TransitionData {
                    popped: 0,
                    buf_size: 3,
                    pushed: vec![],
                }
            }

            fn log_pop(&mut self) {
                if self.pushed.pop().is_none() {
                    self.popped += 1;
                }
            }

            fn log_push(&mut self, state: &'static str) {
                self.pushed.push(state);
            }
        }

        macro_rules! call_now {
            ($_input:expr $( , $arg:expr )* $(,)? ) => {
                return Some({
                    Transition::CallNow(&[ $( ($arg, stringify!($arg)) ),* ])
                })
            };
        }

        macro_rules! call_then {
            ($input:expr $(,  $arg:expr )* $(,)? ) => {
                Some({
                    Transition::CallThen(&[$( ($arg, stringify!($arg)) ),*])
                })
            };
        }

        macro_rules! nothing {
            [$_input:expr] => {
                Some(Transition::CallNow (&[]))
            };
       }

        macro_rules! bump {
            ($input:expr) => {{
                if $input.buffer.is_empty() {
                    false
                } else {
                    true
                }
            }};

            ($input:expr, $token:expr) => {{
                $input.buffer.peek() == $token
            }};
        }

        macro_rules! error {
            ($input:expr) => {{
                return None;
            }};
        }

        macro_rules! end {
            ($input:expr) => {{
                return Some(Transition::CallNow(&[]));
            }};
        }

        macro_rules! cond {
            ($input:expr, $method:ident, $expected:expr) => {{
                $input.buffer.$method() == $expected
            }};
        }
    }
}

pub(crate) fn input_ty() -> TokenStream {
    quote! { &mut RustParser }
}

pub(crate) fn output_ty() -> TokenStream {
    quote! { Option<Transition> }
}
