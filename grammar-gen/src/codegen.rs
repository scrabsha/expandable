pub(crate) mod rt;

use proc_macro2::TokenStream;

use crate::mir::Production;

pub(crate) fn generate_code(productions: Vec<Production>) -> TokenStream {
    let rt = rt::runtime_base();

    let productions = productions
        .into_iter()
        .map(Production::into_token_stream)
        .collect();

    [rt, productions].into_iter().collect()
}
