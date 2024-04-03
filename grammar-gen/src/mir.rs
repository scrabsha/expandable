use std::{cell::RefCell, iter};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::{
    codegen::rt,
    parse::{Block, Builtin, Document, Expr, Function},
};

pub(crate) fn productions_from_document(doc: Document) -> Vec<Production> {
    Production::from_document(&doc)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Production {
    name: Ident,
    kind: ProductionKind,
}

#[derive(Clone, Debug, PartialEq)]
enum ProductionKind {
    Bump {
        descr: Option<Ident>,
        then: Vec<Ident>,
    },
    CallNow {
        then: Vec<Ident>,
    },
    Error,
    Cond {
        builtin: Builtin,
        descr: Option<Ident>,
        cons: Vec<Ident>,
        alt: Vec<Ident>,
    },
}

impl Production {
    pub(crate) fn from_document(doc: &Document) -> Vec<Production> {
        doc.fns
            .iter()
            .flat_map(Production::generate_from_fn)
            .collect()
    }

    fn generate_from_fn(fn_: &Function) -> Vec<Production> {
        let ctxt = GenCtxt::new(fn_.name.clone());

        let (mut fns, entry_point) = Production::for_block(&fn_.body, None, &ctxt);
        fns.push(Production::mk_entry_point(&fn_.name, entry_point));

        fns
    }

    fn for_block(block: &Block, then: Option<Ident>, ctxt: &GenCtxt) -> (Vec<Production>, Ident) {
        let (mut intermediates, entry_point) = block.stmts.iter().rev().fold(
            (Vec::new(), then.clone()),
            |(mut intermediates, then), stmt| {
                let (new_intermediates, entry_point) = Production::for_expr(&stmt.expr, then, ctxt);
                intermediates.extend(new_intermediates);
                (intermediates, Some(entry_point))
            },
        );

        let entry_point = entry_point.unwrap_or_else(|| {
            let (intermediates_, entry_point) = Production::empty_rule(then, ctxt);
            intermediates.extend(intermediates_);
            entry_point
        });

        (intermediates, entry_point)
    }

    fn for_expr(expr: &Expr, then: Option<Ident>, ctxt: &GenCtxt) -> (Vec<Production>, Ident) {
        match expr {
            Expr::Call(block) => {
                let mut then_ = vec![block.func.clone()];
                then_.extend(then);

                let prod = Production {
                    name: ctxt.gensym(),
                    kind: ProductionKind::CallNow { then: then_ },
                };
                let name = prod.name.clone();

                (vec![prod], name)
            }

            Expr::Condition(cond) => {
                let (intermediate_cons, cons_name) =
                    Production::for_block(&cond.consequence, then.clone(), ctxt);

                let (intermediate_alt, alt_name) = cond
                    .alternative
                    .as_ref()
                    .map(|(_, alt)| Production::for_expr(alt, then.clone(), ctxt))
                    .map(|(intermediates, alt_name)| (intermediates, Some(alt_name)))
                    .unwrap_or_default();

                let builtin = cond.cond.builtin.clone();
                let descr = cond
                    .cond
                    .predicate
                    .as_ref()
                    .map(|pred| &pred.ident)
                    .cloned();

                let mut cons = vec![cons_name];
                let mut alt = alt_name.into_iter().collect::<Vec<_>>();

                cons.extend(then.clone());
                alt.extend(then);

                let prod = Production {
                    name: ctxt.gensym(),
                    kind: ProductionKind::Cond {
                        builtin,
                        descr,
                        cons,
                        alt,
                    },
                };
                let name = prod.name.clone();

                let intermediates = iter::once(prod)
                    .chain(intermediate_cons)
                    .chain(intermediate_alt)
                    .collect();

                (intermediates, name)
            }

            Expr::Builtin(builtin) => {
                let descr = builtin.predicate.as_ref().map(|pred| &pred.ident).cloned();
                let then = then.into_iter().collect();
                let kind = match builtin.builtin {
                    Builtin::Bump | Builtin::Read => ProductionKind::Bump { descr, then },
                    Builtin::Error => ProductionKind::Error,

                    _ => panic!("Can't use that builtin in a non-condition"),
                };

                let prod = Production {
                    name: ctxt.gensym(),
                    kind,
                };
                let name = prod.name.clone();

                (vec![prod], name)
            }

            Expr::Block(block) => Production::for_block(block, then, ctxt),
        }
    }

    fn empty_rule(then: Option<Ident>, ctxt: &GenCtxt) -> (Vec<Production>, Ident) {
        let then = then.into_iter().collect();
        let prod = Production {
            name: ctxt.gensym(),
            kind: ProductionKind::CallNow { then },
        };
        let name = prod.name.clone();

        (vec![prod], name)
    }

    pub(crate) fn into_token_stream(self) -> TokenStream {
        let Production { name, kind } = self;
        let input_ty = rt::input_ty();
        let output_ty = rt::output_ty();

        let body = match kind {
            ProductionKind::Bump {
                descr: Some(descr),
                then,
            } => {
                quote! {
                    if bump![input, #descr] {
                        call_then![input, #( #then ),* ]
                    } else {
                        error![input]
                    }
                }
            }

            ProductionKind::Bump { descr: None, then } => {
                quote! {
                    if bump![input] {
                        call_then![input, #( #then),* ]
                    } else {
                        error![input]
                    }
                }
            }

            ProductionKind::CallNow { then } => {
                quote! {
                    call_now![input, #( #then ),* ]
                }
            }

            ProductionKind::Error => quote! { error![input] },

            ProductionKind::Cond {
                builtin,
                descr,
                cons,
                alt,
            } => {
                let builtin_call = codegen_builtin_call(builtin, descr);

                quote! {
                    if #builtin_call {
                        call_now![input, #( #cons ),* ]
                    } else {
                        call_now![input, #( #alt ),* ]
                    }
                }
            }
        };

        quote! {
            fn #name(input: #input_ty) -> #output_ty {
                eprintln!("{}: {:?}", stringify!(#name), input.buffer.peek());
                #body
            }
        }
    }

    fn mk_entry_point(name: &Ident, entry_point: Ident) -> Production {
        let name = name.clone();
        Production {
            name,
            kind: ProductionKind::CallNow {
                then: vec![entry_point],
            },
        }
    }
}

fn codegen_builtin_call(builtin: Builtin, descr: Option<Ident>) -> TokenStream {
    let builtin = match builtin {
        Builtin::Bump => quote! { bump },
        Builtin::Read => quote! { read },
        Builtin::Peek => quote! { peek },
        Builtin::Peek2 => quote! { peek2 },
        Builtin::Peek3 => quote! { peek3 },
        Builtin::Error => quote! { error },
    };

    // It turns out Option<_> does not implement quote's internal iterator
    // trait. I'm sad we have to allocate here, but I see no better solution.
    let descr = descr.into_iter().collect::<Vec<_>>();

    quote! {
        cond![input, #builtin #( , #descr )*]
    }
}

#[derive(Clone, Debug, PartialEq)]
struct GenCtxt {
    base_fn: Ident,
    gensym_counter: RefCell<usize>,
}

impl GenCtxt {
    fn new(base_fn: Ident) -> GenCtxt {
        GenCtxt {
            base_fn,
            gensym_counter: RefCell::default(),
        }
    }

    fn gensym(&self) -> Ident {
        let base_fn = &self.base_fn;
        let mut ref_ = self.gensym_counter.borrow_mut();
        let idx = *ref_;
        *ref_ += 1;

        format_ident!("{base_fn}_{idx}")
    }
}
