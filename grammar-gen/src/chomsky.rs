use std::mem;
use std::sync::Mutex;

use crate::grammar_maker::{Expr, Grammar, Rule};

pub(crate) fn convert(mut input: Grammar) -> Grammar {
    todo!()
}

fn gensym() -> String {
    static COUNTER: Mutex<usize> = Mutex::new(0);

    let mut guard = COUNTER.lock().unwrap();
    *guard += 1;

    format!("Chomsky_{}", guard)
}
