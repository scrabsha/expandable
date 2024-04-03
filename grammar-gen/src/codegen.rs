pub(crate) mod rt;

use std::{cell::RefCell, iter};

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;

use crate::{
    mir::Production,
    parse::{Block, Builtin, BuiltinExpr, CallExpr, CondExpr, Document, Expr, Function, Predicate},
};

pub(crate) fn generate_code(productions: Vec<Production>) -> TokenStream {
    let rt = rt::runtime_base();

    let productions = productions
        .into_iter()
        .map(Production::into_token_stream)
        .collect();

    [rt, productions].into_iter().collect()
}

fn generate_intermediate_fns(fn_: &Function) -> Vec<IntermediateFunction> {
    let ctxt = GenCtxt::new(fn_.name.to_string().as_str());

    let (mut fns, entry_point) = codegen_block(&fn_.body, &ctxt, None);

    fns.push(ctxt.codegen_entry_point(&fn_.name, entry_point));

    fns
}

fn codegen_expr(
    expr: &Expr,
    ctxt: &GenCtxt,
    then: Option<Ident>,
) -> (Vec<IntermediateFunction>, Ident) {
    match expr {
        Expr::Block(block) => codegen_block(block, ctxt, then),

        Expr::Call(CallExpr { func, .. }) => {
            let fn_ = ctxt.codegen_call_now(func.clone(), then);
            let name = fn_.name.clone();

            (vec![fn_], name)
        }

        Expr::Condition(CondExpr {
            cond,
            consequence,
            alternative,
            ..
        }) => {
            let (intermediate_consequences, consequence_name) =
                codegen_block(consequence, ctxt, then.clone());

            let (intermediate_alternatives, alternative_name) = alternative
                .as_ref()
                .map(|(_, alt)| codegen_expr(alt, ctxt, then.clone()))
                .map(|(a, b)| (a, Some(b)))
                .unwrap_or_default();

            let fn_ =
                ctxt.codegen_condition(cond, consequence_name, alternative_name, then.clone());
            let name = fn_.name.clone();

            let intermediates = iter::once(fn_)
                .chain(intermediate_consequences)
                .chain(intermediate_alternatives)
                .collect();

            (intermediates, name)
        }

        Expr::Builtin(BuiltinExpr {
            builtin, predicate, ..
        }) => ctxt.codegen_builtin_call(builtin, predicate, then),
    }
}

fn codegen_block(
    block: &Block,
    ctxt: &GenCtxt,
    then: Option<Ident>,
) -> (Vec<IntermediateFunction>, Ident) {
    let (mut intermediates, fn_) = block.stmts.iter().rev().fold(
        (Vec::new(), then.clone()),
        |(mut intermediates, then), stmt| {
            let (new_intermediates, fn_) = codegen_expr(&stmt.expr, ctxt, then);
            intermediates.extend(new_intermediates);
            (intermediates, Some(fn_))
        },
    );

    let fn_ = match fn_ {
        None => {
            let intermediate = ctxt.codegen_empty_rule(then);
            let fn_ = intermediate.name.clone();
            intermediates.push(intermediate);
            fn_
        }
        Some(fn_) => fn_,
    };

    (intermediates, fn_)
}

pub(crate) struct IntermediateFunction {
    name: Ident,
    arg: Ident,
    arg_ty: TokenStream,
    out_ty: TokenStream,
    body: TokenStream,
}

impl IntermediateFunction {
    fn base(name: Ident, arg: Ident, body: TokenStream) -> IntermediateFunction {
        IntermediateFunction {
            name,
            arg,
            arg_ty: rt::input_ty(),
            out_ty: rt::output_ty(),
            body,
        }
    }

    fn into_token_stream(self) -> TokenStream {
        let IntermediateFunction {
            name,
            arg,
            arg_ty,
            out_ty,
            body,
        } = self;
        quote! {
            fn #name(#arg: #arg_ty) -> #out_ty {
                eprintln!("{}: {:?}", stringify!(#name), #arg.buffer.peek());
                #body
            }
        }
    }
}

struct GenCtxt {
    counter: RefCell<usize>,
    base: String,
    input_name: Ident,
}

impl GenCtxt {
    pub(crate) fn codegen_empty_rule(&self, then: Option<Ident>) -> IntermediateFunction {
        let input = self.input_name();
        let body = match then {
            None => quote! { nothing![#input] },
            Some(then) => quote! { nothing![#input, #then] },
        };

        IntermediateFunction::base(self.gensym(), self.input_name(), body)
    }
}

impl GenCtxt {
    pub(crate) fn codegen_entry_point(
        &self,
        fn_name: &Ident,
        generated_entry_point: Ident,
    ) -> IntermediateFunction {
        let body = quote! {
            #generated_entry_point(input)
        };

        IntermediateFunction::base(fn_name.clone(), self.input_name(), body)
    }
}

impl GenCtxt {
    pub(crate) fn codegen_builtin_call(
        &self,
        builtin: &Builtin,
        pred: &Option<Predicate>,
        then: Option<Ident>,
    ) -> (Vec<IntermediateFunction>, Ident) {
        match builtin {
            Builtin::Bump => self.codegen_bump(pred, then),
            Builtin::Error => self.codegen_error(),

            _ => panic!("Can't use that builtin in a non-condition"),
        }
    }

    fn codegen_bump(
        &self,
        pred: &Option<Predicate>,
        then: Option<Ident>,
    ) -> (Vec<IntermediateFunction>, Ident) {
        let name = self.gensym();
        let name_ = name.clone();

        let body = match (pred, then) {
            (Some(Predicate { ident }), Some(then)) => quote! {
                if bump![input, #ident] {
                    call_then![input, #then]
                } else {
                    error![input]
                }
            },

            (Some(Predicate { ident }), None) => quote! {
                if bump![input, #ident] {
                    call_then![input]
                } else {
                    error![input]
                }
            },

            (None, Some(then)) => quote! {
                if bump![input] {
                    call_then![input, #then]
                } else {
                    error![input]
                }
            },

            (None, None) => quote! {
                if bump![input] {
                    call_then![input]
                } else {
                    error![input]
                }
            },
        };

        let fn_ = IntermediateFunction::base(name, self.input_name(), body);

        (vec![fn_], name_)
    }

    fn codegen_error(&self) -> (Vec<IntermediateFunction>, Ident) {
        let name = self.gensym();
        let name_ = name.clone();

        let body = quote! {
            error![input]
        };

        let fn_ = IntermediateFunction::base(name, self.input_name(), body);

        (vec![fn_], name_)
    }

    pub(crate) fn codegen_condition(
        &self,
        cond: &BuiltinExpr,
        cons: Ident,
        alt: Option<Ident>,
        then: Option<Ident>,
    ) -> IntermediateFunction {
        let name = self.gensym();

        let builtin = match cond.builtin {
            Builtin::Bump => quote! { bump },
            Builtin::Read => quote! { read },
            Builtin::Peek => quote! { peek },
            Builtin::Peek2 => quote! { peek2 },
            Builtin::Peek3 => quote! { peek3 },
            Builtin::Error => quote! { error },
        };

        let cond = match &cond.predicate {
            Some(Predicate { ident }) => quote! {
                cond![input, #builtin, #ident]
            },

            None => quote! {
                cond![input, #builtin]
            },
        };

        let cons = match then.as_ref() {
            None => quote! {
                // call_now![input, #cons]
                return #cons(input)
            },

            Some(then) => quote! {
                call_now![input, #cons, #then]
            },
        };

        let tail = match (alt, then) {
            (Some(alt), Some(tail)) => quote! {
                call_now![input, #alt, #tail]
            },

            (Some(alt), None) => quote! {
                // call_now![input, #alt]
                return #alt(input)
            },

            (None, Some(then)) => quote! {
                // call_now![input, #then]
                return #then(input)
            },

            (None, None) => quote! {
                end![input]
            },
        };

        let body = quote! {
            if #cond {
                #cons
            } #tail
        };

        IntermediateFunction::base(name, self.input_name(), body)
    }

    pub(crate) fn codegen_call_now(
        &self,
        call: Ident,
        then: Option<Ident>,
    ) -> IntermediateFunction {
        let body = match then {
            None => quote! {
                // call_now![input, #call]
                return #call(input);
            },

            Some(then) => quote! {
                call_now![input, #call, #then]
            },
        };

        let name = self.gensym();

        IntermediateFunction::base(name, self.input_name(), body)
    }

    fn new(base: &str) -> GenCtxt {
        GenCtxt {
            counter: RefCell::new(0),
            base: base.to_string(),
            input_name: Ident::new("input", Span::call_site()),
        }
    }

    fn gensym(&self) -> Ident {
        let mut counter = self.counter.borrow_mut();
        let ident = Ident::new(&format!("__{}_{}", self.base, *counter), Span::call_site());
        *counter += 1;

        ident
    }

    fn input_name(&self) -> Ident {
        self.input_name.clone()
    }
}
