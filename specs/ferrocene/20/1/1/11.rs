#! /usr/bin/env -S cargo -Zscript

//! `ty` shall only be followed by `=>`, `,`, `=`, `|`, `;`, `:`, `>`, `>>`,
//! `[`, `{`, `as`, `where`, or a metavariable with the block fragment
//! specifier kind.
//!
//! https://spec.ferrocene.dev/macros.html#fls_boiggrfdyhwh

#![allow(unused)]

macro_rules! test {
    ($_:ty +) => {};
}

fn main() {}
