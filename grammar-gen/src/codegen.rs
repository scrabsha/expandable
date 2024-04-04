pub(crate) mod rt;

use proc_macro2::TokenStream;

use crate::mir::{Production, Vis};

pub(crate) fn generate_code(productions: Vec<Production>) -> TokenStream {
    let entry_points = productions.iter().filter_map(|p| match p.vis {
        Vis::Public => Some(p.name.clone()),
        Vis::Private => None,
    });

    let rt = rt::runtime_base(entry_points);

    let productions = productions
        .into_iter()
        .map(Production::into_token_stream)
        .collect();

    [rt, productions].into_iter().collect()
}
