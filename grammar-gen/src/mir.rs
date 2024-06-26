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
    pub(crate) name: Ident,
    pub(crate) vis: Vis,
    pub(crate) kind: ProductionKind,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Vis {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ProductionKind {
    Bump {
        descr: Option<Ident>,
        then: Vec<Ident>,
    },
    CallNow {
        then: Vec<Ident>,
    },
    Error,
    Cond {
        builtins: Vec<(Builtin, Option<Ident>)>,
        cons: Vec<Ident>,
        alt: Vec<Ident>,
    },
}

impl Production {
    pub(crate) fn from_document(doc: &Document) -> Vec<Production> {
        doc.fns.iter().flat_map(Production::for_fn).collect()
    }

    fn for_fn(fn_: &Function) -> Vec<Production> {
        let ctxt = GenCtxt::new(fn_.name.clone());

        let (mut fns, entry_point) = Production::for_block(&fn_.body, &ctxt);
        let vis = match fn_.pub_ {
            Some(_) => Vis::Public,
            None => Vis::Private,
        };

        fns.push(Production::mk_entry_point(&fn_.name, entry_point, vis));

        fns
    }

    fn for_block(block: &Block, ctxt: &GenCtxt) -> (Vec<Production>, Ident) {
        let (mut intermediates, entry_points) = block.stmts.iter().rev().fold(
            (Vec::new(), Vec::new()),
            |(mut intermediates, mut names), stmt| {
                let (new_intermediates, name) = Production::for_expr(&stmt.expr, ctxt);
                intermediates.extend(new_intermediates);
                names.push(name);
                (intermediates, names)
            },
        );

        let then = entry_points.into_iter().rev().collect();

        let production = Production {
            name: ctxt.gensym(),
            vis: Vis::Private,
            kind: ProductionKind::CallNow { then },
        };
        let name = production.name.clone();

        intermediates.push(production);

        (intermediates, name)
    }

    fn for_expr(expr: &Expr, ctxt: &GenCtxt) -> (Vec<Production>, Ident) {
        match expr {
            Expr::Call(block) => {
                let then = vec![block.func.clone()];

                let prod = Production {
                    name: ctxt.gensym(),
                    vis: Vis::Private,
                    kind: ProductionKind::CallNow { then },
                };
                let name = prod.name.clone();

                (vec![prod], name)
            }

            Expr::Condition(cond) => {
                let (intermediate_cons, cons_name) = Production::for_block(&cond.consequence, ctxt);

                let (intermediate_alt, alt_name) = cond
                    .alternative
                    .as_ref()
                    .map(|(_, alt)| Production::for_expr(alt, ctxt))
                    .map(|(intermediates, alt_name)| (intermediates, Some(alt_name)))
                    .unwrap_or_default();

                let builtin = cond
                    .cond
                    .iter()
                    .map(|b_e| {
                        (
                            b_e.builtin.clone(),
                            b_e.predicate.as_ref().map(|pred| pred.ident.clone()),
                        )
                    })
                    .collect();

                let cons = vec![cons_name];
                let alt = alt_name.into_iter().collect::<Vec<_>>();

                let prod = Production {
                    name: ctxt.gensym(),
                    vis: Vis::Private,
                    kind: ProductionKind::Cond {
                        builtins: builtin,
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
                let then = vec![];
                let kind = match builtin.builtin {
                    Builtin::Bump | Builtin::Read => ProductionKind::Bump { descr, then },
                    Builtin::Error => ProductionKind::Error,

                    _ => panic!("Can't use that builtin in a non-condition"),
                };

                let prod = Production {
                    name: ctxt.gensym(),
                    vis: Vis::Private,
                    kind,
                };
                let name = prod.name.clone();

                (vec![prod], name)
            }

            Expr::Block(block) => Production::for_block(block, ctxt),
        }
    }

    pub(crate) fn into_token_stream(self) -> TokenStream {
        let Production { name, kind, vis: _ } = self;
        let generic = rt::generic();
        let input_ty = rt::input_ty();
        let output_ty = rt::output_ty();

        let body = match kind {
            ProductionKind::Bump {
                descr: Some(descr),
                then,
            } => {
                let then = then.into_iter().rev();

                quote! {
                    input.bump_expect(#descr, &[ #( #then ),* ])
                }
            }

            ProductionKind::Bump { descr: None, then } => {
                let then = then.into_iter().rev();

                quote! {
                    input.bump_noexpect(&[ #( #then ),* ])
                }
            }

            ProductionKind::CallNow { then } => {
                let then = then.into_iter().rev();

                quote! {
                    input.call_now( &[ #( #then ),* ])
                }
            }

            ProductionKind::Error => quote! { input.error() },

            ProductionKind::Cond {
                builtins,
                cons,
                alt,
            } => {
                let builtins = builtins
                    .into_iter()
                    .map(|(b, p)| codegen_builtin_call(b, p));
                let cons = cons.into_iter().rev();
                let alt = alt.into_iter().rev();

                quote! {
                    if #( input.#builtins )||* {
                        input.call_now(&[ #( #cons ),* ])
                    } else {
                        input.call_now(&[ #( #alt ),* ])
                    }
                }
            }
        };

        quote! {
            fn #name<#generic>(input: #input_ty) -> #output_ty {
                // eprintln!("{}: {:?}", stringify!(#name), input.buffer.peek().map(|(k, _)| k));
                #body
            }
        }
    }

    fn mk_entry_point(name: &Ident, entry_point: Ident, vis: Vis) -> Production {
        let name = name.clone();
        Production {
            name,
            vis,
            kind: ProductionKind::CallNow {
                then: vec![entry_point],
            },
        }
    }
}

fn codegen_builtin_call(builtin: Builtin, expect: Option<Ident>) -> TokenStream {
    let builtin = match (builtin, expect.is_some()) {
        (Builtin::Bump, true) => quote! { bump_expect },
        (Builtin::Read, true) => quote! { bump_expect },
        (Builtin::Peek, true) => quote! { peek_expect },
        (Builtin::Peek2, true) => quote! { peek2_expect },
        (Builtin::Peek3, true) => quote! { peek3_expect },
        (Builtin::Error, true) => quote! { error_expect },

        (Builtin::Bump, false) => quote! { bump_noexpect },
        (Builtin::Read, false) => quote! { bump_noexpect },
        (Builtin::Peek, false) => quote! { peek_noexpect },
        (Builtin::Peek2, false) => quote! { peek2_noexpect },
        (Builtin::Peek3, false) => quote! { peek3_noexpect },
        (Builtin::Error, false) => quote! { error_noexpect },
    };

    let expect = expect.into_iter().collect::<Vec<_>>();

    quote! {
        #builtin(#( #expect, )*)
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
