pub(crate) mod rt;

use std::collections::HashSet;

use proc_macro2::TokenStream;
use syn::Ident;

use crate::mir::{Production, ProductionKind, Vis};

pub(crate) fn generate_code(productions: Vec<Production>) -> TokenStream {
    let entry_points = productions.iter().filter_map(|p| match p.vis {
        Vis::Public => Some(p.name.clone()),
        Vis::Private => None,
    });

    check_all_functions_are_defined(&productions);

    let rt = rt::runtime_base(entry_points);

    let productions = productions
        .into_iter()
        .map(Production::into_token_stream)
        .collect();

    [rt, productions].into_iter().collect()
}

fn check_all_functions_are_defined(productions: &[Production]) {
    let fn_names = productions.iter().map(|p| &p.name).collect::<HashSet<_>>();

    let check_all_defined = |fns: &[Ident]| {
        fns.iter()
            .for_each(|fn_| 
                // TODO: panicking here is not a good solution.
                assert!(fn_names.get(fn_).is_some(), "Function {fn_} is not defined")
            )
    };

    for prod in productions {
        match &prod.kind {
            ProductionKind::Bump { descr: _, then } | ProductionKind::CallNow { then } => {
                check_all_defined(then)
            }

            ProductionKind::Error => {}

            ProductionKind::Cond {
                builtin: _,
                descr: _,
                cons,
                alt,
            } => {
                check_all_defined(cons);
                check_all_defined(alt);
            }
        }
    }
}
