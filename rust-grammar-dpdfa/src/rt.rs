use std::{
    cmp::Ordering,
    iter,
    marker::Copy,
    mem,
    ops::{Add, AddAssign, Index, IndexMut, Sub},
};

use smallvec::SmallVec;

use crate::token::TokenDescription;

const PARSING_FUEL: usize = 256;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Program {
    pub(crate) functions: &'static [Function],
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) name: &'static str,
    pub(crate) code: &'static [Instruction],
    // Registers #0..#n are used for function arguments.
    //
    // Next registers are used for local variables.
    pub(crate) reg_num: usize,
}

impl Index<Address> for Function {
    type Output = Instruction;

    fn index(&self, index: Address) -> &Instruction {
        &self.code[index.0 as usize]
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Instruction {
    LoadConst(Register, Value),
    #[expect(dead_code)]
    Copy {
        src: Register,
        dst: Register,
    },
    Sub {
        lhs: Register,
        rhs: Register,
        out: Register,
    },
    #[expect(dead_code)]
    Add {
        lhs: Register,
        rhs: Register,
        out: Register,
    },
    Jump {
        address: Address,
    },
    JumpIfZero {
        cond: Register,
        address: Address,
    },
    JumpIfNonZero {
        cond: Register,
        address: Address,
    },
    Invert {
        src: Register,
        dst: Register,
    },
    #[expect(dead_code)]
    PushArg(Register),
    Call {
        function: FunctionId,
        result: Register,
    },
    Return(Register),
    Peek {
        tok: TokenDescription,
        reg: Register,
    },
    PeekAny {
        reg: Register,
    },
    Peek2 {
        tok: TokenDescription,
        reg: Register,
    },
    #[expect(dead_code)]
    Peek3 {
        tok: TokenDescription,
        reg: Register,
    },
    BumpToken {
        tok: TokenDescription,
    },
    Bump,
    Error {
        message: &'static str,
    },
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Register(pub u8);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Address(pub u32);

impl Address {
    #[expect(dead_code)]
    fn next(self) -> Address {
        Address(self.0 + 1)
    }

    fn prev(self) -> Address {
        Address(self.0 - 1)
    }
}

impl AddAssign<u32> for Address {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct FunctionId(pub(crate) u32);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct Value(pub(crate) i8);

impl Value {
    const ONE: Value = Value(1);
    const ZERO: Value = Value(0);

    fn is_zero(self) -> bool {
        self.0 == 0
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> Value {
        Value(self.0 + rhs.0)
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> Value {
        Value(self.0 - rhs.0)
    }
}

#[derive(Clone, Debug)]
pub struct Interpreter<'code, Span> {
    program: &'code Program,
    // Allows to assign frame ids.
    frame_counter: usize,
    // Allows to assign description ids.
    descr_counter: usize,
    stack: Vec<StackFrame<'code>>,
    token_buffer: TokenBuffer<Span>,
    next_call_args: Vec<Value>,
}

impl<'code, Span> Interpreter<'code, Span>
where
    Span: Copy,
{
    pub(crate) fn new(program: &'code Program, main_fn: FunctionId) -> Interpreter<'code, Span> {
        let mut this = Self {
            program,
            frame_counter: 0,
            descr_counter: 0,
            stack: Vec::new(),
            token_buffer: TokenBuffer::EMPTY,
            next_call_args: Vec::new(),
        };

        let main_frame = this.create_stack_frame(main_fn);
        this.stack.push(main_frame);

        this
    }

    pub fn step(
        &mut self,
        token_description: TokenDescription,
        span: Span,
    ) -> Result<Transition, (Vec<TokenDescription>, Span)> {
        let trans = if self.token_buffer.status() == TokenBufferStatus::Full {
            self.perform_progress()?
        } else {
            Transition::empty(self.token_buffer, self.stack.last().unwrap())
        };

        let reserved_id = self.descr_id();
        self.token_buffer.push(token_description, span, reserved_id);

        Ok(trans)
    }

    pub fn finish(&mut self, end_span: Span) -> Result<Transition, (Vec<TokenDescription>, Span)> {
        // Keep calling `perform_progress` and progressively fill the buffer
        // with `Eof` tokens, until it contains only `Eof` tokens.

        // Safety measure: make sure the buffer is full before doing anything.
        self.fill_buffer(end_span);

        let mut trans = Transition::empty(self.token_buffer, self.stack.last().unwrap());

        let mut finish_fuel = PARSING_FUEL;
        let mut tried = Vec::new();
        while self.token_buffer.first().unwrap().0 != TokenDescription::Eof {
            assert!(finish_fuel > 0, "Out of finishing fuel.");
            finish_fuel -= 1;

            trans += self.perform_progress()?;

            // Safety measure: make sure the buffer is full before doing anything.
            self.fill_buffer(end_span);
        }

        // Now that the buffer is full of EOF tokens (which can't be consumed),
        // we run the interpreter until the stack is empty.
        let mut finish_fuel = PARSING_FUEL;
        tried.clear();
        while !self.stack.is_empty() {
            if finish_fuel == 0 {
                return Err((Vec::new(), end_span));
            }
            finish_fuel -= 1;

            match self.run_instruction(&mut trans, &mut tried) {
                Ok(()) => {}
                Err((_, span)) => return Err((tried, span)),
            };

            // Safety measure: make sure the buffer is full before doing anything.
            self.fill_buffer(end_span);
        }

        Ok(trans)
    }

    fn fill_buffer(&mut self, end_span: Span) {
        while self.token_buffer.status() != TokenBufferStatus::Full {
            let reserved_id = self.descr_id();
            self.token_buffer
                .push(TokenDescription::Eof, end_span, reserved_id);
        }
    }

    fn perform_progress(&mut self) -> Result<Transition, (Vec<TokenDescription>, Span)> {
        let mut fuel = PARSING_FUEL;
        let mut trans = Transition::empty(self.token_buffer, self.stack.last().unwrap());
        let mut tried = Vec::<TokenDescription>::new();

        while self.token_buffer.status() == TokenBufferStatus::Full {
            assert!(fuel > 0, "Out of parsing fuel.");
            fuel -= 1;

            match self.run_instruction(&mut trans, &mut tried) {
                Ok(()) => {}
                Err((_, span)) => return Err((tried, span)),
            }
        }

        Ok(trans)
    }

    fn run_instruction(
        &mut self,
        transition: &mut Transition,
        tried: &mut Vec<TokenDescription>,
    ) -> Result<(), (String, Span)> {
        let current_frame = match self.stack.last_mut() {
            Some(frame) => frame,
            None => {
                return Err((
                    "Expected EOF".to_string(),
                    self.token_buffer.first().unwrap().1,
                ));
            }
        };

        transition.log_frame_usage(current_frame);

        let current_instr = current_frame[current_frame.ip];

        current_frame.ip += 1;

        match current_instr {
            // Regular VM instructions.
            Instruction::LoadConst(reg, val) => {
                current_frame[reg] = val;
            }

            Instruction::Copy { src, dst } => {
                current_frame[dst] = current_frame[src];
            }

            Instruction::Sub { lhs, rhs, out } => {
                current_frame[out] = current_frame[lhs] - current_frame[rhs];
            }

            Instruction::Add { lhs, rhs, out } => {
                current_frame[out] = current_frame[lhs] + current_frame[rhs];
            }

            Instruction::Jump { address } => {
                current_frame.ip = address;
            }

            Instruction::JumpIfZero { cond, address } => {
                if current_frame[cond].is_zero() {
                    current_frame.ip = address;
                }
            }

            Instruction::JumpIfNonZero { cond, address } => {
                if !current_frame[cond].is_zero() {
                    current_frame.ip = address;
                }
            }

            Instruction::Invert { src, dst } => {
                let out_val = if current_frame[src].is_zero() {
                    Value::ONE
                } else {
                    Value::ZERO
                };

                current_frame[dst] = out_val;
            }

            Instruction::PushArg(reg) => {
                self.next_call_args.push(current_frame[reg]);
            }

            Instruction::Call { function, .. } => {
                let new_frame = self.create_stack_frame(function);

                transition.log_frame_usage(&new_frame);
                self.stack.push(new_frame);

                return Ok(());
            }

            Instruction::Return(reg) => {
                let return_value = current_frame[reg];

                self.stack.pop();

                if let Some(frame) = self.stack.last_mut() {
                    transition.log_frame_usage(frame);

                    // Backtrack to get the register in which the return value should be stored.
                    let call_instr = frame[frame.ip.prev()];
                    let return_reg = match call_instr {
                        Instruction::Call { result, .. } => result,
                        _ => unreachable!(),
                    };

                    frame[return_reg] = return_value;

                    // Safety measure against bad codegen :)
                    self.next_call_args.clear();
                }

                return Ok(());
            }

            // Parsing-specific instructions.
            Instruction::Peek { tok, reg } => {
                tried.push(tok);

                let tok_ = self.token_buffer.first().unwrap().0;

                if tok_ == tok || tok_.try_split_with(tok).is_some() {
                    current_frame[reg] = Value::ONE;
                } else {
                    current_frame[reg] = Value::ZERO;
                }
            }

            Instruction::PeekAny { reg } => {
                let any = matches!(self.token_buffer.first(), Some((tok, _)) if tok != TokenDescription::Eof);

                if any {
                    current_frame[reg] = Value::ONE;
                } else {
                    current_frame[reg] = Value::ZERO;
                }
            }

            Instruction::Peek2 { tok, reg } => {
                let tok_ = self.token_buffer.second().unwrap().0;
                if tok_ == tok {
                    current_frame[reg] = Value::ONE;
                } else {
                    current_frame[reg] = Value::ZERO;
                }
            }

            Instruction::Peek3 { tok, reg } => {
                let tok_ = self.token_buffer.third().unwrap().0;
                if tok_ == tok {
                    current_frame[reg] = Value::ONE;
                } else {
                    current_frame[reg] = Value::ZERO;
                }
            }

            Instruction::BumpToken { tok } => {
                tried.push(tok);
                let (tok_, span) = self.token_buffer.first().unwrap();

                if tok_ == tok {
                    self.token_buffer.pop_front();
                } else if let Some(head) = tok_.try_split_with(tok) {
                    let reserved_id = self.descr_id();
                    self.token_buffer.set_head(head, reserved_id);
                } else {
                    return Err(("Unexpected token".to_string(), span));
                }
            }

            Instruction::Bump => {
                self.token_buffer.pop_front();
            }

            Instruction::Error { message } => {
                return Err((message.to_string(), self.token_buffer.first().unwrap().1));
            }
        }

        Ok(())
    }

    fn create_stack_frame(&mut self, function: FunctionId) -> StackFrame<'code> {
        let id = self.frame_id();

        let function = &self.program.functions[function.0 as usize];
        let fn_locals_num = function.reg_num - self.next_call_args.len();

        let registers = iter::once(Value::ZERO) // Return value.
            .chain(mem::take(&mut self.next_call_args)) // Arguments
            .chain(iter::repeat(Value::ZERO).take(fn_locals_num)) // Locals
            .collect::<Vec<_>>();

        StackFrame {
            id,
            function,
            ip: Address(0),
            registers,
        }
    }

    fn frame_id(&mut self) -> StackFrameId {
        let id = self.frame_counter;
        self.frame_counter += 1;
        StackFrameId(id)
    }

    fn descr_id(&mut self) -> DescrId {
        let id_ = DescrId(self.descr_counter);
        self.descr_counter += 1;

        id_
    }

    pub fn initial_transition(&self) -> Transition {
        Transition::empty(self.token_buffer, self.stack.last().unwrap())
    }
}

impl<Span> PartialEq for Interpreter<'_, Span>
where
    Span: Copy,
{
    fn eq(&self, other: &Interpreter<'_, Span>) -> bool {
        // Let's assume we can skip the program part - it will always be the same.

        self.frame_counter.eq(&other.frame_counter)
            && self.descr_counter.eq(&other.descr_counter)
            && self.stack.eq(&other.stack)
            && self.token_buffer.eq(&other.token_buffer)
            && self.next_call_args.eq(&other.next_call_args)
    }
}
impl<Span> Eq for Interpreter<'_, Span> where Span: Copy {}

impl<Span> PartialOrd for Interpreter<'_, Span>
where
    Span: Copy,
{
    fn partial_cmp(&self, other: &Interpreter<'_, Span>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Span> Ord for Interpreter<'_, Span>
where
    Span: Copy,
{
    fn cmp(&self, other: &Interpreter<'_, Span>) -> Ordering {
        // Skipping the `program` field - it is and will always be - the same.
        //
        // Skipping the `*_counter` fields - they don't represent the state of
        // the interpreter - only the state of the future executions.
        self.stack
            .cmp(&other.stack)
            .then_with(|| self.token_buffer.cmp(&other.token_buffer))
            .then_with(|| self.next_call_args.cmp(&other.next_call_args))
    }
}

#[derive(Clone, Debug)]
struct StackFrame<'code> {
    // Each stack frame has its own unique id. This allows us to properly
    // multiple transitions into one.
    id: StackFrameId,
    function: &'code Function,
    ip: Address,
    registers: Vec<Value>,
}

impl<'code> PartialOrd for StackFrame<'code> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'code> Ord for StackFrame<'code> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Skipping the `function` field - it will always be the same.
        //
        // Skipping the `id` field - it will always be the same.
        self.ip
            .cmp(&other.ip)
            .then_with(|| self.registers.cmp(&other.registers))
    }
}

impl<'code> PartialEq for StackFrame<'code> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<'code> Eq for StackFrame<'code> {}

impl<'code> Index<Register> for StackFrame<'code> {
    type Output = Value;

    fn index(&self, index: Register) -> &Value {
        &self.registers[index.0 as usize]
    }
}

impl<'code> IndexMut<Register> for StackFrame<'code> {
    fn index_mut(&mut self, index: Register) -> &mut Value {
        &mut self.registers[index.0 as usize]
    }
}

impl<'code> Index<Address> for StackFrame<'code> {
    type Output = Instruction;

    fn index(&self, index: Address) -> &Instruction {
        &self.function[index]
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct StackFrameId(pub(crate) usize);

#[derive(Clone, Copy, Debug)]
enum TokenBuffer<Span> {
    Zero([TokenBufferSlot<Span>; 0]),
    One([TokenBufferSlot<Span>; 1]),
    Two([TokenBufferSlot<Span>; 2]),
    Three([TokenBufferSlot<Span>; 3]),
}

impl<Span> Eq for TokenBuffer<Span> where Span: Copy {}

impl<Span> PartialEq for TokenBuffer<Span>
where
    Span: Copy,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_slice().eq(other.as_slice())
    }
}

impl<Span> PartialOrd for TokenBuffer<Span>
where
    Span: Copy,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Span> Ord for TokenBuffer<Span>
where
    Span: Copy,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct DescrId(usize);

impl<Span> TokenBuffer<Span>
where
    Span: Copy,
{
    const EMPTY: TokenBuffer<Span> = TokenBuffer::Zero([]);

    fn as_slice(&self) -> &[TokenBufferSlot<Span>] {
        match self {
            TokenBuffer::Zero(inner) => inner,
            TokenBuffer::One(inner) => inner,
            TokenBuffer::Two(inner) => inner,
            TokenBuffer::Three(inner) => inner,
        }
    }

    fn status(&self) -> TokenBufferStatus {
        match self {
            TokenBuffer::Zero(_) => TokenBufferStatus::Empty,
            TokenBuffer::One(_) => TokenBufferStatus::One,
            TokenBuffer::Two(_) => TokenBufferStatus::Two,
            TokenBuffer::Three(_) => TokenBufferStatus::Full,
        }
    }

    fn push(&mut self, descr: TokenDescription, span: Span, id: DescrId) {
        let slot = TokenBufferSlot { descr, span, id };

        match self {
            TokenBuffer::Zero([]) => {
                *self = TokenBuffer::One([slot]);
            }
            TokenBuffer::One([a]) => {
                *self = TokenBuffer::Two([*a, slot]);
            }
            TokenBuffer::Two([a, b]) => {
                *self = TokenBuffer::Three([*a, *b, slot]);
            }
            TokenBuffer::Three(_) => {
                panic!("Token buffer is full");
            }
        }
    }

    fn first(&self) -> Option<(TokenDescription, Span)> {
        match self.as_slice() {
            [] => None,
            [TokenBufferSlot { descr, span, .. }, ..] => Some((*descr, *span)),
        }
    }

    pub(crate) fn second(&self) -> Option<(TokenDescription, Span)> {
        match self.as_slice() {
            [] | [_] => None,
            [_, TokenBufferSlot { descr, span, .. }, ..] => Some((*descr, *span)),
        }
    }

    pub(crate) fn third(&self) -> Option<(TokenDescription, Span)> {
        match self.as_slice() {
            [] | [_] | [_, _] => None,
            [_, _, TokenBufferSlot { descr, span, .. }, ..] => Some((*descr, *span)),
        }
    }

    fn pop_front(&mut self) {
        match self {
            TokenBuffer::Zero([]) => unreachable!(),

            TokenBuffer::One([_, remaining @ ..]) => {
                *self = TokenBuffer::Zero(*remaining);
            }

            TokenBuffer::Two([_, remaining @ ..]) => {
                *self = TokenBuffer::One(*remaining);
            }

            TokenBuffer::Three([_, remaining @ ..]) => {
                *self = TokenBuffer::Two(*remaining);
            }
        }
    }

    fn set_head(&mut self, descr_: TokenDescription, id_: DescrId) {
        match self {
            TokenBuffer::Zero([]) => panic!("`set_head` called en empty buffer"),

            TokenBuffer::One([TokenBufferSlot { descr, id, .. }])
            | TokenBuffer::Two([TokenBufferSlot { descr, id, .. }, _])
            | TokenBuffer::Three([TokenBufferSlot { descr, id, .. }, _, _]) => {
                *descr = descr_;
                *id = id_;
            }
        }
    }

    fn as_small_vec(&self) -> SmallVec<[(TokenDescription, DescrId); 16]> {
        self.as_slice()
            .iter()
            .copied()
            .map(TokenBufferSlot::forget_span)
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
struct TokenBufferSlot<Span> {
    descr: TokenDescription,
    span: Span,
    id: DescrId,
}

impl<Span> PartialEq for TokenBufferSlot<Span> {
    fn eq(&self, other: &TokenBufferSlot<Span>) -> bool {
        (self.descr, self.id).eq(&(other.descr, other.id))
    }
}

impl<Span> Eq for TokenBufferSlot<Span> {}

impl<Span> Ord for TokenBufferSlot<Span> {
    fn cmp(&self, other: &TokenBufferSlot<Span>) -> Ordering {
        // Skipping the `span` field. We are generic over this :).
        //
        // Skipping the `id` field - it is only used for token deduplication
        // when two transitions are added.
        self.descr.cmp(&other.descr)
    }
}

impl<Span> PartialOrd for TokenBufferSlot<Span> {
    fn partial_cmp(&self, other: &TokenBufferSlot<Span>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Span> TokenBufferSlot<Span> {
    fn forget_span(self) -> (TokenDescription, DescrId) {
        (self.descr, self.id)
    }
}

#[derive(Debug, PartialEq)]
enum TokenBufferStatus {
    Empty,
    One,
    Two,
    Full,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Transition {
    // All the tokens that have been used for the transition, including those
    // in the token buffer.
    buffer: SmallVec<[(TokenDescription, DescrId); 16]>,
    // The top of the stack that is used for the transition. Reversed.
    stack_top_rev: Vec<FrameDescr>,
}

impl Transition {
    fn empty<Span>(buffer: TokenBuffer<Span>, stack_top: &StackFrame) -> Transition
    where
        Span: Copy,
    {
        let buffer = buffer.as_small_vec();
        let top = FrameDescr::for_frame(stack_top);

        Transition {
            buffer,
            stack_top_rev: vec![top],
        }
    }

    /// Must be called before the frame usage.
    ///
    /// Do not call this with a frame that has been popped earlier.
    fn log_frame_usage(&mut self, frame: &StackFrame) {
        if frame.id < self.stack_top_rev.last().unwrap().id {
            // The used frame is older (smaller id). It is therefore a parent of
            // the top frame. We need to log it.

            self.stack_top_rev.push(FrameDescr::for_frame(frame));
        }
    }
}

impl AddAssign for Transition {
    fn add_assign(&mut self, rhs: Transition) {
        // We find the first token that is common to both the `self` and `rhs`,
        // then we push everything that comes after in `rhs` to `self`.
        let last_known_descr_id = self.buffer.last().map(|(_, id)| id);

        let to_append = match last_known_descr_id {
            Some(id) => {
                let idx_to_start_from = rhs
                    .buffer
                    .iter()
                    .enumerate()
                    .find(|(_, (_, id_))| id_ == id)
                    .map(|(idx, _)| idx);

                match idx_to_start_from {
                    Some(idx) if idx + 1 < rhs.buffer.len() => &rhs.buffer[idx + 1..],
                    Some(_) => &[],
                    None => &rhs.buffer,
                }
            }
            None => rhs.buffer.as_slice(),
        };

        self.buffer.extend_from_slice(to_append);

        let mut to_append = rhs
            .stack_top_rev
            .into_iter()
            .filter(|frame| self.stack_top_rev.last().unwrap().id > frame.id)
            .collect();

        self.stack_top_rev.append(&mut to_append);
    }
}

impl Add for Transition {
    type Output = Transition;

    fn add(mut self, rhs: Transition) -> Transition {
        self += rhs;
        self
    }
}

impl PartialOrd for Transition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Transition {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.buffer
            .iter()
            .map(|(descr, _)| descr)
            .cmp(rhs.buffer.iter().map(|(descr, _)| descr))
            .then_with(|| {
                self.buffer
                    .iter()
                    .map(|(descr, _)| descr)
                    .cmp(rhs.buffer.iter().map(|(descr, _)| descr))
            })
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct FrameDescr {
    id: StackFrameId,
    // The IP at the beginning of the transition.
    ip: Address,
    // The value of the registers at the beginning of the transition.
    registers: Vec<Value>,
}

impl FrameDescr {
    fn for_frame(frame: &StackFrame) -> FrameDescr {
        FrameDescr {
            id: frame.id,
            ip: frame.ip,
            registers: frame.registers.clone(),
        }
    }
}
