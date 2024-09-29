use proc_macro2::Ident;
use smallvec::SmallVec;
use syn::{
    Error, Result, Token, Type, parenthesized,
    parse::Parse,
    punctuated::Punctuated,
    token::{self, Let, Paren, Pub},
};

pub(crate) fn parse(content: &str) -> Result<Document> {
    syn::parse_str(content)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Document {
    pub(crate) fns: Vec<Function>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) pub_: Option<Pub>,
    pub(crate) fn_token: Token![fn],
    pub(crate) name: Ident,
    pub(crate) signature: Signature,
    pub(crate) body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Signature {
    pub(crate) paren_token: Paren,
    pub(crate) args: Vec<Ident>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Block {
    pub(crate) brace_token: syn::token::Brace,
    pub(crate) stmts: Vec<Stmt>,
    pub(crate) ret: Option<Return>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Stmt {
    pub(crate) declare: Option<(Let, Ident, Token![=])>,
    pub(crate) expr: Expr,
    pub(crate) semi: Option<Token![;]>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Return {
    pub(crate) ret: Token![return],
    pub(crate) expr: Box<Expr>,
    pub(crate) semi: Token![;],
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Call(CallExpr),
    Condition(CondExpr),
    Builtin(BuiltinExpr),
    Block(Block),
    Binop(BinopExpr),
    Neg(NegExpr),
    Ident(IdentExpr),
    Paren(ParenExpr),
    Loop(LoopExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CallExpr {
    pub(crate) func: Ident,
    pub(crate) paren: Paren,
    pub(crate) args: SmallVec<[Ident; 2]>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CondExpr {
    pub(crate) if_: Token![if],
    pub(crate) cond: Box<Expr>,
    pub(crate) consequence: Block,
    pub(crate) alternative: Option<(Token![else], Box<Expr>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct BinopExpr {
    pub(crate) lhs: Box<Expr>,
    pub(crate) op: Binop,
    pub(crate) rhs: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct NegExpr {
    pub(crate) bang: Token![!],
    pub(crate) inner: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct IdentExpr {
    pub(crate) ident: Ident,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Binop {
    LogicAnd,
    LogicOr,
    Assign,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct BuiltinExpr {
    pub(crate) builtin: Builtin,
    pub(crate) paren: Paren,
    pub(crate) predicate: SmallVec<[Predicate; 2]>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Builtin {
    Bump,
    Read,
    Peek,
    Peek2,
    Peek3,
    Error,
    Returned,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Predicate {
    pub(crate) ident: Ident,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ParenExpr {
    pub(crate) paren: token::Paren,
    pub(crate) inner: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LoopExpr {
    pub(crate) loop_: Token![loop],
    pub(crate) body: Block,
}

impl Parse for ParenExpr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let inner;

        Ok(ParenExpr {
            paren: parenthesized!(inner in input),
            inner: Box::new(inner.parse()?),
        })
    }
}

impl Parse for Document {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut fns = Vec::new();
        while !input.is_empty() {
            fns.push(input.parse()?);
        }
        Ok(Document { fns })
    }
}

impl Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Function {
            pub_: input.parse()?,
            fn_token: input.parse()?,
            name: input.parse()?,
            signature: input.parse()?,
            body: input.parse()?,
        })
    }
}

impl Parse for Signature {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner;
        Ok(Signature {
            paren_token: parenthesized!(inner in input),
            args: {
                struct Arg {
                    name: Ident,
                    _colon: Token![:],
                    _ty: Type,
                }

                impl Parse for Arg {
                    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
                        Ok(Arg {
                            name: input.parse()?,
                            _colon: input.parse()?,
                            _ty: input.parse()?,
                        })
                    }
                }

                Punctuated::<Arg, Token![,]>::parse_terminated(&inner)?
                    .into_iter()
                    .map(|arg| arg.name)
                    .collect()
            },
        })
    }
}

impl Parse for Block {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Block {
            brace_token: syn::braced!(content in input),
            stmts: {
                let mut stmts = Vec::new();
                while !content.is_empty() && !content.peek(Token![return]) {
                    stmts.push(content.parse()?);
                }
                stmts
            },
            ret: {
                let ret = if content.peek(Token![return]) {
                    Some(content.parse()?)
                } else {
                    None
                };

                if !content.is_empty() {
                    return Err(content.error("Expected end of block"));
                }

                ret
            },
        })
    }
}

impl Parse for Stmt {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let declare = if input.peek(Token![let]) {
            Some((input.parse()?, input.parse()?, input.parse()?))
        } else {
            None
        };

        let expr = input.parse()?;
        let semi = match &expr {
            Expr::Condition(_) | Expr::Loop(_) => None,
            _ => Some(input.parse()?),
        };

        Ok(Stmt {
            declare,
            expr,
            semi,
        })
    }
}

impl Parse for Return {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Return {
            ret: input.parse()?,
            expr: Box::new(Expr::parse_atom(input)?),
            semi: input.parse()?,
        })
    }
}

impl Parse for Expr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let expr = Expr::parse_ors(input)?;

        Ok(if input.peek(Token![=]) {
            let lhs = expr;

            input.parse::<Token![=]>().unwrap();

            let rhs = input.parse::<Expr>()?;

            Expr::Binop(BinopExpr {
                lhs: Box::new(lhs),
                op: Binop::Assign,
                rhs: Box::new(rhs),
            })
        } else {
            expr
        })
    }
}

impl Expr {
    fn parse_ors(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let op = Binop::LogicOr;
        let sep = Token![||];
        let inner: fn(syn::parse::ParseStream) -> syn::Result<Self> = Self::parse_ands;
        let mut lhs = inner(input)?;

        while input.peek(sep) {
            input.parse::<Token![||]>().unwrap();

            let rhs = inner(input)?;

            lhs = Expr::Binop(BinopExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }

    fn parse_ands(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let op = Binop::LogicAnd;
        let sep = Token![&&];
        let inner: fn(syn::parse::ParseStream) -> syn::Result<Self> = Self::parse_atom;
        let mut lhs = inner(input)?;

        while input.peek(sep) {
            input.parse::<Token![&&]>().unwrap();

            let rhs = inner(input)?;

            lhs = Expr::Binop(BinopExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }

    fn parse_atom(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(token::Brace) {
            Expr::Block(input.parse()?)
        } else if input.peek(kw::bump)
            || input.peek(kw::read)
            || input.peek(kw::peek)
            || input.peek(kw::peek2)
            || input.peek(kw::peek3)
            || input.peek(kw::error)
            || input.peek(kw::returned)
        {
            Expr::Builtin(input.parse()?)
        } else if input.peek(Token![if]) {
            Expr::Condition(input.parse()?)
        } else if input.peek(Token![loop]) {
            Expr::Loop(input.parse()?)
        } else if input.peek(Token![!]) {
            Expr::Neg(input.parse()?)
        } else if input.peek(token::Paren) {
            Expr::Paren(input.parse()?)
        } else {
            // Distinguish between function call and ident expr.
            if input.peek2(token::Paren) {
                Expr::Call(input.parse()?)
            } else {
                Expr::Ident(input.parse()?)
            }
        })
    }
}

impl Parse for CallExpr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _inner;
        Ok(CallExpr {
            func: input.parse()?,
            paren: parenthesized!(_inner in input),
            args: {
                let tmp: Punctuated<Ident, Token![,]> = Punctuated::parse_terminated(&_inner)?;
                tmp.into_pairs().map(|pair| pair.into_value()).collect()
            },
        })
    }
}

impl Parse for CondExpr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(CondExpr {
            if_: input.parse()?,
            cond: input.parse()?,
            consequence: input.parse()?,
            alternative: {
                if input.peek(Token![else]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                }
            },
        })
    }
}

impl Parse for BuiltinExpr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let builtin = input.parse()?;
        let inner;
        Ok(BuiltinExpr {
            builtin,
            paren: parenthesized!(inner in input),
            predicate: {
                let tmp: Punctuated<Predicate, Token![,]> = Punctuated::parse_terminated(&inner)?;
                tmp.into_pairs().map(|pair| pair.into_value()).collect()
            },
        })
    }
}

impl Parse for Builtin {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "bump" => Ok(Builtin::Bump),
            "read" => Ok(Builtin::Read),
            "peek" => Ok(Builtin::Peek),
            "peek2" => Ok(Builtin::Peek2),
            "peek3" => Ok(Builtin::Peek3),
            "error" => Ok(Builtin::Error),
            "returned" => Ok(Builtin::Returned),
            _ => Err(Error::new(
                ident.span(),
                "expected one of `bump`, `read`, `peek`, `peek2`, `peek3`, `error`, `returned`",
            )),
        }
    }
}

impl Parse for Predicate {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Predicate {
            ident: input.parse()?,
        })
    }
}

impl Parse for NegExpr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        Ok(NegExpr {
            bang: input.parse()?,
            inner: Box::new(input.parse()?),
        })
    }
}

impl Parse for IdentExpr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        Ok(IdentExpr {
            ident: input.parse()?,
        })
    }
}

impl Parse for LoopExpr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        Ok(LoopExpr {
            loop_: input.parse()?,
            body: input.parse()?,
        })
    }
}

mod kw {
    syn::custom_keyword!(bump);
    syn::custom_keyword!(read);
    syn::custom_keyword!(peek);
    syn::custom_keyword!(peek2);
    syn::custom_keyword!(peek3);
    syn::custom_keyword!(error);
    syn::custom_keyword!(returned);
}
