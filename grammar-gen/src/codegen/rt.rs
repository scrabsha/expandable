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
        use std::cmp::Ordering;

        use smallvec::SmallVec;

        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

        impl TokenDescription {
            fn try_split_with(self, first: TokenDescription) -> Option<TokenDescription> {
                match (self, first) {
                    // `&&` -> `&` and `&`
                    (AndAnd, And) => Some(And),
                    // `||` -> `|` and `|`
                    (OrOr, Or) => Some(Or),
                    // `<<` -> `<` and `<`
                    (Shl, LessThan) => Some(LessThan),
                    // `>>` -> `>` and `>`
                    (Shr, GreaterThan) => Some(GreaterThan),
                    // `+=` -> `+` and `=`
                    (PlusEquals, Plus) => Some(Equals),
                    // `-=` -> `-` and `=`
                    (MinusEquals, Minus) => Some(Equals),
                    // `*=` -> `*` and `=`
                    (StarEquals, Star) => Some(Equals),
                    // `/=` -> `/` and `=`
                    (SlashEquals, Slash) => Some(Equals),
                    // `%=` -> `%` and `=`
                    (PercentEquals, Percent) => Some(Equals),
                    // `^=` -> `^` and `=`
                    (CaretEquals, Caret) => Some(Equals),
                    // `&=` -> `&` and `=`
                    (AndEquals, And) => Some(Equals),
                    // `|=` -> `|` and `=`
                    (OrEquals, Or) => Some(Equals),
                    // `<<=` -> `<<` and `=`
                    (ShlEquals, Shl) => Some(Equals),
                    // `<<=` -> `<` and `<=`
                    (ShrEquals, Shr) => Some(LessThanEquals),
                    // `>>=` -> `>>` and `=`
                    (ShrEquals, Shr) => Some(Equals),
                    // `>>=` -> `>` and `>=`
                    (ShrEquals, Shr) => Some(GreaterThanEquals),
                    // `..` -> `.` and `.`
                    (DotDot, Dot) => Some(Dot),

                    // TODO: add remaining cases if necessary.
                    _ => None,

                }
            }
        }

        #[derive(Clone, Debug)]
        pub struct RustParser<Span>
        where
            Span: 'static + Copy,
        {
            buffer: TokenBuffer<Span>,
            stack: Vec<State<Span>>,
            // TODO: is this appropriate?
            tried: SmallVec<[TokenDescription; 10]>,
        }

        impl<Span> PartialEq for RustParser<Span>
        where
            Span: Copy + 'static
        {
            fn eq(&self, other: &RustParser<Span>) -> bool {
                std::ptr::eq(self, other)
                || (self.buffer == other.buffer && self.stack == other.stack)
            }
        }

        impl<Span: Copy + 'static> Eq for RustParser<Span> {}

        impl<Span> Hash for RustParser<Span>
        where
            Span: 'static + Copy
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.buffer.hash(state);
                self.stack.hash(state);
            }
        }

        impl<Span> Ord for RustParser<Span>
        where
            Span: Copy,
        {
            fn cmp(&self, other: &RustParser<Span>) -> Ordering {
                self
                    .stack
                    .len()
                    .cmp(&other.stack.len())
                    .then_with(|| self.stack.iter().rev().cmp(other.stack.iter().rev()))
                    .then_with(|| self.buffer.cmp(&other.buffer))
            }
        }

        impl<Span> PartialOrd for RustParser<Span>
        where
            Span: Copy + 'static
        {
            fn partial_cmp(&self, other: &RustParser<Span>) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct TransitionData {
            pub popped: usize,
            pub buf_size: usize,
            // TODO: wrap this in an opaque type.
            pub pushed: Vec<TypeErasedState>,
        }

        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct TypeErasedState {
            inner: *const (),
        }

        impl<Span> RustParser<Span>
        where
            Span: Copy + 'static,
        {
            #(
                pub fn #entry_points() -> RustParser<Span> {
                    RustParser {
                        buffer: TokenBuffer::Empty([]),
                        stack: vec![#entry_points],
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
                    let state = self.stack.pop().ok_or_else(|| {
                        let s = self.buffer.peek().map(|(_, s)| s);
                        ProgressError::EmptyStack(s)
                    })?;

                    match state(self).map_err(|s| {
                        ProgressError::ParsingError(s, self.tried.clone().into_vec())
                    })? {
                        Transition::CallNow(states) => {
                            states.iter().copied().for_each(|f| {
                                let state = TypeErasedState {
                                    inner: f as *const (),
                                };
                                trans.log_push(state);
                            });
                            self.stack.extend(states.iter().copied());
                        },
                        Transition::CallThen(states) => {
                            self.buffer.shift();
                            states.iter().cloned().for_each(|f| {
                                let state = TypeErasedState {
                                    inner: f as *const (),
                                };
                                trans.log_push(state);
                            });
                            self.stack.extend(states.iter().copied());
                            break Ok(trans);
                        },
                    }
                }
            }

            fn bump_expect(
                &mut self,
                descr: TokenDescription,
                then: &'static [State<Span>]
            ) -> Result<Transition<Span>, Option<Span>> {
                self.tried.push(descr);

                match self.buffer.peek() {
                    Some((descr_, sp)) if descr_ == descr => Ok(Transition::CallThen(&[])),
                    Some((descr_, sp)) => match descr_.try_split_with(descr) {
                        Some(replacement) => {
                            self.buffer.replace_first(replacement);
                            Ok(Transition::CallNow(then))
                        },
                        None => Err(Some(sp)),
                    },

                    _ => Err(None),
                }
            }

            fn bump_noexpect(&mut self, then: &'static [State<Span>]) -> Result<Transition<Span>, Option<Span>> {
                // TODO: this may eat more tokens than required. Be careful!
                if self.buffer.peek().is_some() {
                    Ok(Transition::CallThen(&[]))
                } else {
                    Err(None)
                }
            }

            fn peek_expect(&mut self, descr: TokenDescription) -> bool {
                self.run_cond_fn_expect(TokenBuffer::peek, descr)
            }

            fn peek2_expect(&mut self, descr: TokenDescription) -> bool {
                self.run_cond_fn_expect(TokenBuffer::peek2, descr)
            }

            fn peek3_expect(&mut self, descr: TokenDescription) -> bool {
                self.run_cond_fn_expect(TokenBuffer::peek3, descr)
            }

            fn peek_noexpect(&mut self) -> bool {
                self.run_cond_fn_noexpect(TokenBuffer::peek)
            }

            fn peek2_noexpect(&mut self) -> bool {
                self.run_cond_fn_noexpect(TokenBuffer::peek2)
            }

            fn peek3_noexpect(&mut self) -> bool {
                self.run_cond_fn_noexpect(TokenBuffer::peek3)
            }

            fn run_cond_fn_expect<F>(
                &mut self,
                f: F,
                descr: TokenDescription,
            ) -> bool
            where
                F: FnOnce(&TokenBuffer<Span>) -> Option<(TokenDescription, Span)>
            {
                self.tried.push(descr);

                match f(&self.buffer) {
                    Some((descr_, sp)) if descr_ == descr => true,
                    Some((descr_, sp)) => {
                        descr_.try_split_with(descr).is_some()
                    }
                    otherwise => false,
                }
            }

            fn run_cond_fn_noexpect<F>(
                &mut self,
                f: F,
            ) -> bool
            where
                F: FnOnce(&TokenBuffer<Span>) -> Option<(TokenDescription, Span)>
            {
                f(&self.buffer).is_some()
            }

            // We return an OK because it is easier for the codegen
            fn call_now(&mut self, now: &'static [State<Span>]) -> Result<Transition<Span>, Option<Span>> {
                Ok(Transition::CallNow(now))
            }

            // We return an OK because it is easier for the codegen
            fn call_then(&mut self, then: &'static [State<Span>]) -> Result<Transition<Span>, Option<Span>> {
                Ok(Transition::CallThen(then))
            }

            // We return an OK because it is easier for the codegen
            fn error(&self) -> Result<Transition<Span>, Option<Span>> {
                let sp = self.buffer.peek().map(|(_, sp)| sp);

                Err(sp)
            }
        }

        #[derive(Clone, Debug, PartialEq)]
        enum ProgressError<Span> {
            // TODO: should be Span (no option)
            EmptyStack(Option<Span>),
            ParsingError(Option<Span>, Vec<TokenDescription>),
        }

        #[derive(Clone, Debug)]
        enum TokenBuffer<Span>
        where
            Span: Copy + 'static,
        {
            Empty([(TokenDescription, Span); 0]),
            Single([(TokenDescription, Span); 1]),
            Double([(TokenDescription, Span); 2]),
            Triple([(TokenDescription, Span); 3]),
        }

        impl<Span> Hash for TokenBuffer<Span>
        where
            Span: Copy + 'static,
        {
            fn hash<H: Hasher>(&self, state: &mut H) {
                let discriminant = mem::discriminant(self);
                discriminant.hash(state);
                self.tokens().for_each(|t| t.hash(state));
            }
        }

        impl<Span> PartialEq for TokenBuffer<Span>
        where
            Span: Copy + 'static
        {
            fn eq(&self, other: &TokenBuffer<Span>) -> bool {
                let self_tag = mem::discriminant(self);
                let arg1_tag = mem::discriminant(other);
                self_tag == arg1_tag && self.tokens().eq(other.tokens())
            }
        }


        impl<Span: Copy> Eq for TokenBuffer<Span> {}

        impl<Span> Ord for TokenBuffer<Span>
        where
            Span: Copy + 'static
        {
            fn cmp(&self, other: &TokenBuffer<Span>) -> Ordering {
                self
                    .len()
                    .cmp(&other.len())
                    .then_with(|| self.tokens().cmp(other.tokens()))
            }
        }

        impl<Span> PartialOrd for TokenBuffer<Span>
        where
            Span: Copy + 'static
        {
            fn partial_cmp(&self, other: &TokenBuffer<Span>) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        type State<Span> = fn(&mut RustParser<Span>) -> Result<Transition<Span>, Option<Span>>;

        #[derive(Clone, Copy, Debug, PartialEq)]
        enum Transition<Span>
        where
            Span: Copy + 'static
        {
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

            fn replace_first(&mut self, descr: TokenDescription) {
                match self {
                    TokenBuffer::Empty(_) => unreachable!(),

                    TokenBuffer::Single([(a, _)])
                    | TokenBuffer::Double([(a, _), _])
                    | TokenBuffer::Triple([(a, _), _, _]) => *a = descr,
                }
            }

            fn tokens(&self) -> impl Iterator<Item = TokenDescription> + '_ {
                self.as_slice().iter().map(|(descr, _)| descr).copied()
            }

            fn as_slice(&self) -> &[(TokenDescription, Span)] {
                match self {
                    TokenBuffer::Empty(b) => b,
                    TokenBuffer::Single(b) => b,
                    TokenBuffer::Double(b) => b,
                    TokenBuffer::Triple(b) => b,
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

            fn log_push(&mut self, state: TypeErasedState) {
                self.pushed.push(state);
            }
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
