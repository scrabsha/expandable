#! /usr/bin/env -S cargo -Zscript

//! If the repetition operator is `*` or `+`, then the possible beginnings of the
//! `MacroRepetitionMatchContent` shall be allowed by its `MacroRepetitionMatchContent`'s fragment
//! specifier restrictions.
//!
//! https://spec.ferrocene.dev/macros.html#fls_rdvs8dz6ouu7

#![allow(unused)]

macro_rules! test {
    ($(- $e:expr)*) => {};
}

fn main() {}
