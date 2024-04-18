use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

pub(crate) fn runtime_base(entry_points: impl IntoIterator<Item = Ident>) -> TokenStream {
    let entry_points = entry_points.into_iter();

    quote! {
        #![allow(
            unused,
            non_snake_case,
        )]

        use std::hash::{Hash, Hasher};
        use std::mem;

        use smallvec::SmallVec;

        /// A single token.
        ///
        /// We are not interested in the exact token, but rather in what kind
        /// of token it is. For instance, both `foo` and `bar` are [`Ident`].
        ///
        /// This also includes the different types of fragment.
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

        #[derive(Clone, Debug)]
        pub struct RustParser<Span: 'static> {
            buffer: TokenBuffer<Span>,
            stack: Vec<State<Span>>,
            // TODO: is this appropriate?
            tried: SmallVec<[TokenDescription; 10]>,
        }

        impl<Span> PartialEq for RustParser<Span> {
            fn eq(&self, other: &RustParser<Span>) -> bool {
                std::ptr::eq(self, other)
                || (self.buffer == other.buffer && self.stack == other.stack)
            }
        }

        impl<Span> Eq for RustParser<Span> {}

        impl<Span> Hash for RustParser<Span> where Span: 'static {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.buffer.hash(state);
                self.stack.hash(state);
            }
        }

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct TransitionData {
            pub popped: usize,
            pub buf_size: usize,
            // TODO: wrap this in an opaque type.
            pub pushed: Vec<&'static str>,
        }

        impl<Span> RustParser<Span>
        where
            Span: Copy + 'static,
        {
            #(
                pub fn #entry_points() -> RustParser<Span> {
                    RustParser {
                        buffer: TokenBuffer::Empty([]),
                        stack: vec![(#entry_points, concat!("<", stringify!(#entry_points), " entry point>"))],
                        tried: SmallVec::new(),
                    }
                }
            )*

            pub fn step(&mut self, token: TokenDescription, span: Span) -> Result<TransitionData, (Span, Vec<TokenDescription>)> {
                match self.buffer.len() {
                    buf_size @ (0 | 1) => {
                        self.buffer.push(token, span);
                        return Ok(TransitionData {
                            popped: 0,
                            buf_size,
                            pushed: vec![],
                        });
                    },

                    2 => {
                        self.buffer.push(token, span);
                    },

                    _ => panic!("Token buffer is full!"),
                };

                self.perform_progress().map_err(|e| match e {
                        // None spans arise when we are parsing the leftover
                        // tokens. Here, we know the buffer size is 2
                        // (2 tokens initially + 1 pushed - 1 eaten while
                        // performing progress).
                    ProgressError::EmptyStack(s) => (s.unwrap(), vec![]),
                    | ProgressError::ParsingError(s, expected) => {
                        (s.unwrap(), expected)
                    }
                })
            }

            pub fn finish(&mut self) -> Result<(), Option<(Span, Vec<TokenDescription>)>> {
                while !self.buffer.is_empty() {
                    match self.perform_progress() {
                        Ok(_) => continue,
                        // TODO: we need to disambuate "expected EOF" and
                        // "this is not the right token".
                        Err(ProgressError::EmptyStack(s)) => {
                            let e = s.map(|s| (s, vec![]));
                            return Err(e);
                        },

                        Err(ProgressError::ParsingError(s, e)) => {
                            return Err(s.map(|s| (s, e)));
                        },
                    }

                }

                while !self.stack.is_empty() {
                    match self.perform_progress() {
                        Ok(_) => continue,
                        Err(ProgressError::EmptyStack(_)) => break,
                        Err(ProgressError::ParsingError(s, e)) => {
                            let e = s.map(|s| (s, e));
                            return Err(e);
                        },
                    }
                }

                Ok(())
            }

            fn perform_progress(&mut self) -> Result<TransitionData, ProgressError<Span>> {
                let mut trans = TransitionData::new_full_buffer();
                self.tried.clear();

                let mut fuel = u8::MAX;
                loop {
                    assert!(fuel != 0, "Out of fuel. This is probably a bug in the parser codegen.");
                    fuel -= 1;

                    // Expected eof, found `...`
                    trans.log_pop();
                    let (state, name) = self.stack.pop().ok_or_else(|| {
                        let s = self.buffer.peek().map(|(_, s)| s);
                        ProgressError::EmptyStack(s)
                    })?;

                    match state(self).map_err(|s| {
                        ProgressError::ParsingError(s, self.tried.clone().into_vec())
                    })? {
                        Transition::CallNow(states) => {
                            states.iter().for_each(|(_, fn_)| trans.log_push(fn_));
                            self.stack.extend(states.iter().copied());
                        },
                        Transition::CallThen(states) => {
                            self.buffer.shift();
                            states.iter().for_each(|(_, fn_)| trans.log_push(fn_));
                            self.stack.extend(states.iter().copied());
                            break Ok(trans);
                        },
                    }
                }
            }
        }

        #[derive(Clone, Debug, PartialEq)]
        enum ProgressError<Span> {
            // TODO: should be Span (no option)
            EmptyStack(Option<Span>),
            ParsingError(Option<Span>, Vec<TokenDescription>),
        }

        #[derive(Clone, Debug)]
        enum TokenBuffer<Span> {
            Empty([(TokenDescription, Span); 0]),
            Single([(TokenDescription, Span); 1]),
            Double([(TokenDescription, Span); 2]),
            Triple([(TokenDescription, Span); 3]),
        }

        impl<Span> Hash for TokenBuffer<Span> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                let discriminant = mem::discriminant(self);
                discriminant.hash(state);
                match self {
                    TokenBuffer::Empty([]) => <[TokenDescription; 0] as Hash>::hash(&[], state),
                    TokenBuffer::Single([(a, _)]) => [a].hash(state),
                    TokenBuffer::Double([(a, _), (b, _)]) => [a, b].hash(state),
                    TokenBuffer::Triple([(a, _), (b, _), (c, _)]) => [a, b, c].hash(state),
                }
            }
        }

        impl<Span> PartialEq for TokenBuffer<Span> {
            fn eq(&self, other: &TokenBuffer<Span>) -> bool {
                let self_tag = mem::discriminant(self);
                let arg1_tag = mem::discriminant(other);
                self_tag == arg1_tag &&
                match (self, other) {
                    (TokenBuffer::Empty([]), TokenBuffer::Empty([])) => true,

                    (TokenBuffer::Single([(a, _)]), TokenBuffer::Single([(a_, _)]))
                        => [a] == [a_],
                    (TokenBuffer::Double([(a, _), (b, _)]), TokenBuffer::Double([(a_, _), (b_, _)]))
                        => [a, b] == [a_, b_],

                    (
                        TokenBuffer::Triple([(a, _), (b, _), (c, _)]),
                        TokenBuffer::Triple([(a_, _), (b_, _), (c_, _)])
                    ) => [a, b, c] == [a_, b_, c_],

                    _ => unreachable!(),
                }
            }
        }


        impl<Span> Eq for TokenBuffer<Span> {}

        type State<Span> = (fn(&mut RustParser<Span>) -> Result<Transition<Span>, Option<Span>>, &'static str);

        #[derive(Clone, Copy, Debug, PartialEq)]
        enum Transition<Span: 'static> {
            // No token has been consumed
            CallNow(&'static [State<Span>]),
            // Exactly one token has been consumed
            CallThen(&'static [State<Span>]),
        }

        impl<Span> TokenBuffer<Span>
        where
            Span: Copy + 'static,
        {
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

            fn push(&mut self, token: TokenDescription, span: Span) {
                match self {
                    TokenBuffer::Empty([]) => {
                        *self = TokenBuffer::Single([(token, span)]);
                    },
                    TokenBuffer::Single([a]) => {
                        *self = TokenBuffer::Double([*a, (token, span)]);
                    },
                    TokenBuffer::Double([a, b]) => {
                        *self = TokenBuffer::Triple([*a, *b, (token, span)]);
                    },
                    TokenBuffer::Triple([a, b, c]) => {
                        panic!("Token buffer is full!");
                    },
                }
            }

            fn shift(&mut self) -> (TokenDescription, Span) {
                match self {
                    TokenBuffer::Empty(_) => panic!("Token buffer is empty!!"),
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

            fn peek(&self) -> Option<(TokenDescription, Span)> {
                match self {
                    TokenBuffer::Empty(_) => None,

                    TokenBuffer::Single([a])
                    | TokenBuffer::Double([a, _])
                    | TokenBuffer::Triple([a, _, _]) => Some(*a),
                }
            }

            fn peek2(&self) -> Option<(TokenDescription, Span)> {
                match self {
                    TokenBuffer::Empty(_)
                    | TokenBuffer::Single([_]) => None,

                    TokenBuffer::Double([_, a])
                    | TokenBuffer::Triple([_, a, _]) => Some(*a),
                }
            }

            fn peek3(&self) -> Option<(TokenDescription, Span)> {
                match self {
                    TokenBuffer::Empty(_)
                    | TokenBuffer::Single([_])
                    | TokenBuffer::Double([_, _]) => None,

                    TokenBuffer::Triple([_, _, a]) => Some(*a),
                }
            }
        }

        impl TransitionData {
            pub fn empty() -> TransitionData {
                TransitionData {
                    popped: 0,
                    buf_size: 0,
                    pushed: vec![],
                }
            }

            pub fn combine_chasles(mut self, other: TransitionData) -> TransitionData {
                for _ in 0..other.popped {
                    self.log_pop();
                }

                for pushed in other.pushed {
                    self.log_push(pushed);
                }

                self
            }

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
                return Ok({
                    Transition::CallNow(&[ $( ($arg, stringify!($arg)) ),* ])
                })
            };
        }

        macro_rules! call_then {
            ($input:expr $(,  $arg:expr )* $(,)? ) => {
                Ok({
                    Transition::CallThen(&[$( ($arg, stringify!($arg)) ),*])
                })
            };
        }

        macro_rules! nothing {
            [$_input:expr] => {
                Ok(Transition::CallNow (&[]))
            };
       }

        macro_rules! bump {
            ($input:expr) => {{
                !$input.buffer.is_empty()
            }};

            ($input:expr, $token:expr) => {{
                // TODO: please refactor this before the release
                $input.tried.push($token);
                $input.buffer.peek().map(|(k, _)| k == $token).unwrap_or_default()
            }};
        }

        macro_rules! error {
            ($input:expr) => {{
                return Err($input.buffer.peek().map(|(_, span)| span));
            }};
        }

        macro_rules! end {
            ($input:expr) => {{
                return Ok(Transition::CallNow(&[]));
            }};
        }

        macro_rules! cond {
            ($input:expr, $method:ident) => {{
                // TODO: why did I write this?
                !$input.buffer.is_empty()
            }};

            ($input:expr, peek, $expected:expr) => {{
                // TODO: please refactor this before the release
                $input.tried.push($expected);
                $input.buffer.peek().map(|(k, _)| k == $expected).unwrap_or_default()
            }};

            ($input:expr, $method:ident, $expected:expr) => {{
                $input.buffer.$method().map(|(k, _)| k == $expected).unwrap_or_default()
            }};
        }
    }
}

pub(crate) fn input_ty() -> TokenStream {
    quote! { &mut RustParser<Span> }
}

pub(crate) fn output_ty() -> TokenStream {
    quote! { Result<Transition<Span>, Option<Span>> }
}

pub(crate) fn generic() -> TokenStream {
    quote! { Span: Copy }
}
