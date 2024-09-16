pub(crate) mod rt;

use std::{cell::RefCell, collections::HashMap};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{punctuated::Punctuated, Ident, Token};

use crate::parse::{Block, Builtin, BuiltinExpr, Document, Expr, Function, Predicate, Stmt};

pub(crate) fn to_stream(program: Program) -> TokenStream {
    let fns = program.fns.iter().map(EncodableFunction::to_stream);
    let initial_states = program.initial_states.iter().map(InitialState::to_stream);

    quote! {
        use crate::token::TokenDescription;
        use crate::rt::{Instruction, Register, Address, FunctionId, Value, Program, Function, Interpreter};

        const PROGRAM: Program = Program {
            functions: &[ #(#fns),* ]
        };

        #( #initial_states )*
    }
}

pub(crate) fn codegen_document(doc: &Document) -> Program {
    let mut gctxt = GlobalCtxt::new();

    for fn_ in &doc.fns {
        gctxt.add_function(fn_.name.to_string());
    }

    let fns = doc.fns.iter().map(|fn_| codegen_fn(&gctxt, fn_)).collect();

    let initial_states = doc
        .fns
        .iter()
        .filter_map(|fn_| {
            if fn_.pub_.is_some() {
                Some(gctxt.initial_state(fn_))
            } else {
                None
            }
        })
        .collect();

    Program {
        fns,
        initial_states,
    }
}

fn codegen_fn(gctxt: &GlobalCtxt, f: &Function) -> EncodableFunction {
    let mut ctxt = CodegenCtxt::new(gctxt);

    let name = f.name.to_string();
    let ret_reg = ctxt.register();

    for parameter in &f.signature.args {
        ctxt.variable(&parameter.to_string());
    }

    codegen_block(&mut ctxt, &f.body, ret_reg, None);
    ctxt.cg_return(ret_reg);

    let code = ctxt.encode();

    EncodableFunction {
        name,
        code,
        reg_num: ctxt.reg_num,
    }
}

fn codegen_block(
    ctxt: &mut CodegenCtxt<'_>,
    block: &Block,
    ret_reg: Register,
    jump_at_end: Option<LabelRef>,
) {
    block
        .stmts
        .iter()
        .for_each(|stmt| codegen_stmt(ctxt, stmt, ret_reg));

    if let Some(ret) = &block.ret {
        let val = ctxt.atom_val(&ret.symbol.to_string());
        let val = Value(val.0);
        ctxt.cg_load_const(ret_reg, val);
        ctxt.cg_return(ret_reg);
    } else if let Some(label) = jump_at_end {
        ctxt.cg_jump(label);
    }
}

fn codegen_stmt(ctxt: &mut CodegenCtxt, stmt: &Stmt, ret_reg: Register) {
    codegen_expr(ctxt, &stmt.expr, ret_reg);
}

fn codegen_builtin(ctxt: &mut CodegenCtxt, builtin: &BuiltinExpr) {
    let report_wrong_argcount = |builtin, left: usize, right: usize| {
        panic!("{builtin:?}: expected {right} arguments but got {left}")
    };

    match (builtin.builtin, builtin.predicate.as_slice()) {
        (Builtin::Bump, [pred]) => {
            ctxt.cg_bump_token(&pred.ident);
        }

        (Builtin::Bump, []) => {
            ctxt.cg_bump();
        }

        (Builtin::Bump, args) => report_wrong_argcount(Builtin::Bump, args.len(), 1),

        (Builtin::Read, _) => unreachable!(),

        (Builtin::Peek, [pred]) => {
            let reg = ctxt.register();
            ctxt.cg_peek(reg, &pred.ident);
        }

        (Builtin::Peek2, [pred]) => {
            let reg = ctxt.register();
            ctxt.cg_peek2(reg, &pred.ident);
        }

        (Builtin::Peek3, [pred]) => {
            let reg = ctxt.register();
            ctxt.cg_peek3(reg, &pred.ident);
        }

        (builtin @ (Builtin::Peek | Builtin::Peek2 | Builtin::Peek3), args) => {
            report_wrong_argcount(builtin, args.len(), 1)
        }

        (Builtin::Error, [_, ..]) => unreachable!(),

        (Builtin::Error, []) => {
            ctxt.cg_error();
        }

        (Builtin::Returned, _) => {
            unreachable!()
        }
    }
}

fn codegen_expr(ctxt: &mut CodegenCtxt<'_>, expr: &Expr, ret_reg: Register) {
    match expr {
        Expr::Call(call) => {
            let fn_ = ctxt.fn_id(&call.func.to_string());

            for arg in &call.args {
                let arg_reg = ctxt.get_variable(&arg.to_string());
                ctxt.cg_push_arg(arg_reg);
            }

            let ret_reg = match &call.assign {
                Some((_, id, _)) => ctxt.variable(&id.to_string()),
                None => ctxt.register(),
            };

            ctxt.cg_call(fn_, ret_reg);
        }

        Expr::Condition(cond) => {
            let cond_reg = ctxt.register();
            codegen_condition_eval(ctxt, &cond.cond, cond_reg);

            match &cond.alternative {
                Some((_, expr)) => {
                    let if_false = ctxt.label();
                    let after_cond_eval = ctxt.label();

                    ctxt.cg_jump_if_zero(cond_reg, if_false);
                    codegen_block(ctxt, &cond.consequence, ret_reg, Some(after_cond_eval));
                    ctxt.set_label(if_false);
                    codegen_expr(ctxt, expr.as_ref(), ret_reg);
                    ctxt.set_label(after_cond_eval);
                }

                None => {
                    let if_false = ctxt.label();

                    ctxt.cg_jump_if_zero(cond_reg, if_false);
                    codegen_block(ctxt, &cond.consequence, ret_reg, None);
                    ctxt.set_label(if_false);
                }
            }
        }

        Expr::Builtin(builtin) => {
            codegen_builtin(ctxt, builtin);
        }
        Expr::Block(block) => codegen_block(ctxt, block, ret_reg, None),
    };
}

fn codegen_condition_eval(
    ctxt: &mut CodegenCtxt,
    cond: &Punctuated<BuiltinExpr, Token![||]>,
    reg: Register,
) {
    let report_wrong_argcount = |builtin, left: usize, right: usize| {
        panic!("{builtin:?}: expected {right} arguments but got {left}")
    };

    ctxt.cg_load_const(reg, Value(0));

    let after_eval = ctxt.label();

    for cond in cond {
        match (cond.builtin, cond.predicate.as_slice()) {
            (Builtin::Bump, _) => unreachable!(),
            (Builtin::Read, _) => unreachable!(),

            (Builtin::Peek, [pred]) => {
                ctxt.cg_peek(reg, &pred.ident);
            }

            (Builtin::Peek, []) => {
                ctxt.cg_peek_any(reg);
            }

            (Builtin::Peek2, [pred]) => {
                ctxt.cg_peek2(reg, &pred.ident);
            }

            (Builtin::Peek3, [pred]) => {
                ctxt.cg_peek3(reg, &pred.ident);
            }

            (builtin @ (Builtin::Peek | Builtin::Peek2 | Builtin::Peek3), preds) => {
                report_wrong_argcount(builtin, preds.len(), 1)
            }

            (Builtin::Error, _) => unreachable!(),

            (Builtin::Returned, [lhs, rhs]) => {
                let is_atom = |pred: &Predicate| {
                    pred.ident
                        .to_string()
                        .chars()
                        .next()
                        .unwrap()
                        .is_uppercase()
                };

                match (is_atom(lhs), is_atom(rhs)) {
                    (true, true) => {
                        panic!("Attempt to compare two atoms: {}, {}", lhs.ident, rhs.ident)
                    }

                    (true, false) => codegen_atom_and_variable_comparison(ctxt, lhs, rhs, reg),
                    (false, true) => codegen_atom_and_variable_comparison(ctxt, rhs, lhs, reg),

                    (false, false) => panic!("Open an issue if this is *really* needed"),
                }
            }

            (Builtin::Returned, preds) => report_wrong_argcount(Builtin::Returned, preds.len(), 2),
        }

        ctxt.cg_jump_if_nonzero(reg, after_eval);
    }

    ctxt.set_label(after_eval);
}

fn codegen_atom_and_variable_comparison(
    ctxt: &mut CodegenCtxt,
    atom: &Predicate,
    variable: &Predicate,
    reg: Register,
) {
    let atom = ctxt.atom_val(&atom.ident.to_string());
    let lhs_reg = ctxt.get_variable(&variable.ident.to_string());
    let rhs_reg = ctxt.register();
    ctxt.cg_atom_id_load(atom, rhs_reg);
    ctxt.cg_sub(lhs_reg, rhs_reg, reg);
    ctxt.cg_invert(reg, reg);
}

/// Holds all the internal state of codegen for a single function.
struct CodegenCtxt<'global> {
    reg_num: usize,
    labels: Vec<usize>,
    instrs: Vec<Instruction<LabelRef>>,
    global_ctxt: &'global GlobalCtxt,
    variables: HashMap<String, Register>,
}

impl<'global> CodegenCtxt<'global> {
    fn new(global_ctxt: &'global GlobalCtxt) -> CodegenCtxt<'global> {
        CodegenCtxt {
            reg_num: 0,
            labels: Vec::new(),
            instrs: Vec::new(),
            global_ctxt,
            variables: HashMap::new(),
        }
    }

    /// Allocates a new register
    fn register(&mut self) -> Register {
        let r = self.reg_num;
        self.reg_num += 1;

        Register(r as _)
    }

    fn atom_val(&self, at: &str) -> AtomId {
        self.global_ctxt.atom_value(at)
    }

    fn label(&mut self) -> LabelRef {
        let id = self.labels.len();
        let ref_ = LabelRef(id);
        self.labels.push(0);

        ref_
    }

    fn set_label(&mut self, label: LabelRef) {
        let address = self.instrs.len();
        let LabelRef(id) = label;
        self.labels[id] = address;
    }

    fn fn_id(&self, name: &str) -> FunctionId {
        self.global_ctxt.fn_id(name)
    }

    fn variable(&mut self, name: &str) -> Register {
        let reg = self.register();
        let prev = self.variables.insert(name.to_string(), reg);
        assert!(prev.is_none());
        reg
    }

    fn get_variable(&self, name: &str) -> Register {
        match self.variables.get(name) {
            Some(reg) => *reg,
            None => {
                panic!("variable not found: {name}")
            }
        }
    }

    fn encode(&self) -> Vec<Instruction<Address>> {
        self.instrs
            .iter()
            .map(|instr| self.encode_instr(*instr))
            .collect()
    }

    fn cg_atom_id_load(&mut self, atom: AtomId, reg: Register) {
        let val = Value(atom.0);
        self.cg_load_const(reg, val);
    }

    fn encode_instr(&self, instr: Instruction<LabelRef>) -> Instruction<Address> {
        match instr {
            Instruction::LoadConst(reg, val) => Instruction::LoadConst(reg, val),
            Instruction::Copy { src, dst } => Instruction::Copy { src, dst },
            Instruction::Sub { lhs, rhs, out } => Instruction::Sub { lhs, rhs, out },
            Instruction::Add { lhs, rhs, out } => Instruction::Add { lhs, rhs, out },
            Instruction::JumpIfZero { cond, address } => Instruction::JumpIfZero {
                cond,
                address: Address(self.labels[address.0] as _),
            },
            Instruction::JumpIfNonZero { cond, address } => Instruction::JumpIfNonZero {
                cond,
                address: Address(self.labels[address.0] as _),
            },
            Instruction::Invert { src, dst } => Instruction::Invert { src, dst },
            Instruction::Jump { address } => Instruction::Jump {
                address: Address(self.labels[address.0] as _),
            },
            Instruction::PushArg(reg) => Instruction::PushArg(reg),
            Instruction::Call { function, result } => Instruction::Call { function, result },
            Instruction::Return(reg) => Instruction::Return(reg),
            Instruction::Peek { tok, reg } => Instruction::Peek { tok, reg },
            Instruction::PeekAny { reg } => Instruction::PeekAny { reg },
            Instruction::Peek2 { tok, reg } => Instruction::Peek2 { tok, reg },
            Instruction::Peek3 { tok, reg } => Instruction::Peek3 { tok, reg },
            Instruction::BumpToken { tok } => Instruction::BumpToken { tok },
            Instruction::Bump => Instruction::Bump,
            Instruction::Error => Instruction::Error,
        }
    }
}

macro_rules! codegen_fns {
    ($(fn $name:ident( $($arg:ident : $ty:ty ),* $(,)? ) = $e:expr)*) => {
        impl<'global> CodegenCtxt<'global> {
            $(
                fn $name(&mut self, $( $arg: $ty),* ) {
                    self.instrs.push($e);
                }
            )*
        }
    };
}

codegen_fns! {
    fn cg_return(reg: Register) = Instruction::Return(reg)

    fn cg_call(function: FunctionId, result: Register) = Instruction::Call { function, result }

    fn cg_load_const(reg: Register, val: Value) = Instruction::LoadConst(reg, val)

    fn cg_jump(address: LabelRef) = {
        Instruction::Jump { address }
    }

    fn cg_jump_if_zero(cond: Register, address: LabelRef) = {
        Instruction::JumpIfZero { cond, address }
    }

    fn cg_jump_if_nonzero(cond: Register, address: LabelRef) = {
        Instruction::JumpIfNonZero { cond, address }
    }

    fn cg_peek(reg: Register, descr: &Ident) = {
        let tok = TokenDescription::from_str(&descr.to_string());
        Instruction::Peek { tok, reg }

    }

    fn cg_peek_any(reg: Register) = Instruction::PeekAny { reg }

    fn cg_peek2(reg: Register, descr: &Ident) = {
        let tok = TokenDescription::from_str(&descr.to_string());
        Instruction::Peek2 { tok, reg }
    }

    fn cg_peek3(reg: Register, descr: &Ident) = {
        let tok = TokenDescription::from_str(&descr.to_string());
        Instruction::Peek3 { tok, reg }
    }

    fn cg_bump() = Instruction::Bump

    fn cg_bump_token(descr: &Ident) = {
        let tok = TokenDescription::from_str(&descr.to_string());
        Instruction::BumpToken { tok }
    }

    fn cg_error() = Instruction::Error {}

    fn cg_sub(lhs: Register, rhs: Register, out: Register) = Instruction::Sub { lhs, rhs, out }

    fn cg_invert(src: Register, dst: Register) = Instruction::Invert { src, dst }

    fn cg_push_arg(reg: Register) = Instruction::PushArg(reg)
}

struct GlobalCtxt {
    fns: HashMap<String, FunctionId>,
    labels: RefCell<HashMap<String, AtomId>>,
}

impl GlobalCtxt {
    fn new() -> GlobalCtxt {
        GlobalCtxt {
            fns: HashMap::new(),
            labels: RefCell::new(HashMap::new()),
        }
    }

    fn add_function(&mut self, name: String) {
        let id = self.fns.len().try_into().unwrap();
        let id = FunctionId(id);
        let prev = self.fns.insert(name, id);
        assert!(prev.is_none());
    }

    fn fn_id(&self, name: &str) -> FunctionId {
        self.fns[name]
    }

    fn atom_value(&self, name: &str) -> AtomId {
        let labels = self.labels.borrow();

        match labels.get(name) {
            Some(v) => *v,
            None => {
                let id = AtomId(labels.len().try_into().unwrap());
                drop(labels);

                let name = name.to_string();
                let mut labels = self.labels.borrow_mut();
                labels.insert(name, id);

                id
            }
        }
    }

    fn initial_state(&self, fn_: &Function) -> InitialState {
        InitialState {
            name: fn_.name.clone(),
            function_id: self.fn_id(&fn_.name.to_string()),
        }
    }
}

pub(crate) struct Program {
    fns: Vec<EncodableFunction>,
    initial_states: Vec<InitialState>,
}

struct EncodableFunction {
    name: String,
    code: Vec<EncodableInstruction>,
    // Registers #0..#n are used for function arguments.
    //
    // Next registers are used for local variables.
    reg_num: usize,
}

impl EncodableFunction {
    fn to_stream(&self) -> TokenStream {
        let code = self
            .code
            .iter()
            .cloned()
            .map(EncodableInstruction::to_stream);
        let reg_num = self.reg_num;
        let name = &self.name;

        quote! {
            Function {
                name: #name,
                code: &[ #(#code),* ],
                reg_num: #reg_num,
            }
        }
    }
}

type EncodableInstruction = Instruction<Address>;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Instruction<Addr> {
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
    JumpIfZero {
        cond: Register,
        address: Addr,
    },
    JumpIfNonZero {
        cond: Register,
        address: Addr,
    },
    Invert {
        src: Register,
        dst: Register,
    },
    Jump {
        address: Addr,
    },
    PushArg(Register),
    Call {
        function: FunctionId,
        result: Register,
    },
    Return(Register),
    PeekAny {
        reg: Register,
    },
    Peek {
        tok: TokenDescription,
        reg: Register,
    },
    Peek2 {
        tok: TokenDescription,
        reg: Register,
    },
    Peek3 {
        tok: TokenDescription,
        reg: Register,
    },
    BumpToken {
        tok: TokenDescription,
    },
    Bump,
    Error,
}

impl EncodableInstruction {
    fn to_stream(self) -> TokenStream {
        match self {
            Instruction::LoadConst(reg, val) => {
                let Register(reg) = reg;
                let Value(val) = val;
                quote! { Instruction::LoadConst(Register(#reg), Value(#val)) }
            }

            Instruction::Copy { src, dst } => {
                let Register(src) = src;
                let Register(dst) = dst;
                quote! { Instruction::Copy { src: Register(#src), dst: Register(#dst) } }
            }

            Instruction::Sub { lhs, rhs, out } => {
                let Register(lhs) = lhs;
                let Register(rhs) = rhs;
                let Register(out) = out;
                quote! { Instruction::Sub { lhs: Register(#lhs), rhs: Register(#rhs), out: Register(#out) } }
            }

            Instruction::Add { lhs, rhs, out } => {
                let Register(lhs) = lhs;
                let Register(rhs) = rhs;
                let Register(out) = out;
                quote! { Instruction::Add { lhs: Register(#lhs), rhs: Register(#rhs), out: Register(#out) } }
            }

            Instruction::JumpIfZero { cond, address } => {
                let Register(cond) = cond;
                let Address(address) = address;
                quote! { Instruction::JumpIfZero { cond: Register(#cond), address: Address(#address) } }
            }

            Instruction::JumpIfNonZero { cond, address } => {
                let Register(cond) = cond;
                let Address(address) = address;
                quote! { Instruction::JumpIfNonZero { cond: Register(#cond), address: Address(#address) } }
            }

            Instruction::Invert { src, dst } => {
                let Register(src) = src;
                let Register(dst) = dst;
                quote! { Instruction::Invert { src: Register(#src), dst: Register(#dst) } }
            }

            Instruction::Jump { address } => {
                let Address(address) = address;
                quote! { Instruction::Jump { address: Address(#address) } }
            }

            Instruction::PushArg(reg) => {
                let Register(reg) = reg;
                quote! { Instruction::PushArg(Register(#reg)) }
            }

            Instruction::Call { function, result } => {
                let FunctionId(function) = function;
                let Register(result) = result;
                quote! { Instruction::Call { function: FunctionId(#function), result: Register(#result) } }
            }

            Instruction::Return(reg) => {
                let Register(reg) = reg;
                quote! { Instruction::Return(Register(#reg)) }
            }

            Instruction::Peek { tok, reg } => {
                let Register(reg) = reg;
                quote! { Instruction::Peek { tok: TokenDescription::#tok, reg: Register(#reg) } }
            }

            Instruction::PeekAny { reg } => {
                let Register(reg) = reg;
                quote! { Instruction::PeekAny { reg: Register(#reg) } }
            }

            Instruction::Peek2 { tok, reg } => {
                let Register(reg) = reg;
                quote! { Instruction::Peek2 { tok: TokenDescription::#tok, reg: Register(#reg) } }
            }

            Instruction::Peek3 { tok, reg } => {
                let Register(reg) = reg;
                quote! { Instruction::Peek3 { tok: TokenDescription::#tok, reg: Register(#reg) } }
            }

            Instruction::BumpToken { tok } => {
                quote! { Instruction::BumpToken { tok: TokenDescription::#tok } }
            }

            Instruction::Bump => {
                quote! { Instruction::Bump }
            }

            Instruction::Error => {
                quote! { Instruction::Error }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct FunctionId(u32);

#[derive(Clone, Copy, Debug, PartialEq)]
struct AtomId(i8);

#[derive(Clone, Copy, Debug, PartialEq)]
struct Register(u8);

#[derive(Clone, Copy, Debug, PartialEq)]
struct Address(u32);

#[derive(Clone, Copy, Debug, PartialEq)]
struct Value(i8);

#[derive(Clone, Copy, Debug, PartialEq)]
struct LabelRef(/* index in the labels vec */ usize);

macro_rules! to_tokens_enum {
    (
        $(#[$meta:meta])*
        $vis:vis enum
        $name:ident { $($(#[$variant_meta:meta])* $variant:ident),* $(,)? }
    ) => {
        $(#[$meta])*
        $vis enum $name {
            $(
                $(
                    #[$variant_meta])*
                    $variant
            ),*
        }

        impl quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                match self {
                    $(
                        $name::$variant => quote::ToTokens::to_tokens(&quote! { $variant }, tokens),
                    )*
                }
            }
        }

        impl $name {
            fn from_str(s: &str) -> $name {
                match s {
                    $(
                        stringify!($variant) => $name::$variant,
                    )*

                    _ => panic!("unknown token: {s}"),
                }
            }
        }
    };
}

to_tokens_enum! {
    #[derive(Clone, Copy, Debug, PartialEq)]
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
        FragmentPatParam,
        FragmentPath,
        FragmentStmt,
        FragmentTt,
        FragmentTy,
        FragmentVis,

        Eof,
    }
}

#[derive(Clone)]
struct InitialState {
    name: Ident,
    function_id: FunctionId,
}

impl InitialState {
    fn to_stream(&self) -> TokenStream {
        let name = self.name.to_string();
        let builder_name = format_ident!("new_{name}");
        let FunctionId(id) = self.function_id;

        quote! {
            pub fn #builder_name<Span>() -> Interpreter<'static, Span>
            where
                Span: Copy,
            {
                Interpreter::new(&PROGRAM, FunctionId(#id))
            }
        }
    }
}
