use proc_macro2::Ident;
use syn::{parenthesized, parse::Parse, token, token::Paren, Error, Result, Token};

pub(crate) fn parse(content: &str) -> Result<Document> {
    syn::parse_str(content)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Document {
    pub(crate) fns: Vec<Function>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) fn_token: Token![fn],
    pub(crate) name: Ident,
    pub(crate) signature: Signature,
    pub(crate) body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Signature {
    pub(crate) paren_token: Paren,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Block {
    pub(crate) brace_token: syn::token::Brace,
    pub(crate) stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Stmt {
    pub(crate) expr: Expr,
    pub(crate) semi: Option<Token![;]>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Call(CallExpr),
    Condition(CondExpr),
    Builtin(BuiltinExpr),
    Block(Block),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CallExpr {
    pub(crate) func: Ident,
    pub(crate) paren: Paren,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CondExpr {
    pub(crate) if_: Token![if],
    pub(crate) cond: BuiltinExpr,
    pub(crate) consequence: Block,
    pub(crate) alternative: Option<(Token![else], Box<Expr>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct BuiltinExpr {
    pub(crate) builtin: Builtin,
    pub(crate) paren: Paren,
    pub(crate) predicate: Option<Predicate>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Builtin {
    Bump,
    Read,
    Peek,
    Peek2,
    Peek3,
    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Predicate {
    pub(crate) ident: Ident,
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
            fn_token: input.parse()?,
            name: input.parse()?,
            signature: input.parse()?,
            body: input.parse()?,
        })
    }
}

impl Parse for Signature {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _inner;
        Ok(Signature {
            paren_token: parenthesized!(_inner in input),
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
                while !content.is_empty() {
                    stmts.push(content.parse()?);
                }
                stmts
            },
        })
    }
}

impl Parse for Stmt {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let expr = input.parse()?;
        let semi = match &expr {
            Expr::Condition(_) => None,
            _ => Some(input.parse()?),
        };

        Ok(Stmt { expr, semi })
    }
}

impl Parse for Expr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if input.peek(token::Brace) {
            Ok(Expr::Block(input.parse()?))
        } else if lookahead.peek(kw::bump) {
            Ok(Expr::Builtin(input.parse()?))
        } else if lookahead.peek(kw::read) {
            Ok(Expr::Builtin(input.parse()?))
        } else if lookahead.peek(kw::peek) {
            Ok(Expr::Builtin(input.parse()?))
        } else if lookahead.peek(kw::peek2) {
            Ok(Expr::Builtin(input.parse()?))
        } else if lookahead.peek(kw::peek3) {
            Ok(Expr::Builtin(input.parse()?))
        } else if lookahead.peek(kw::error) {
            Ok(Expr::Builtin(input.parse()?))
        } else if lookahead.peek(Token![if]) {
            Ok(Expr::Condition(input.parse()?))
        } else {
            Ok(Expr::Call(input.parse()?))
        }
    }
}

impl Parse for CallExpr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _inner;
        Ok(CallExpr {
            func: input.parse()?,
            paren: parenthesized!(_inner in input),
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
                if inner.is_empty() {
                    None
                } else {
                    Some(inner.parse()?)
                }
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
            _ => Err(Error::new(
                ident.span(),
                "expected one of `bump`, `read`, `peek`, `peek2`, `peek3`, `error`",
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

mod kw {
    syn::custom_keyword!(bump);
    syn::custom_keyword!(read);
    syn::custom_keyword!(peek);
    syn::custom_keyword!(peek2);
    syn::custom_keyword!(peek3);
    syn::custom_keyword!(error);
}
