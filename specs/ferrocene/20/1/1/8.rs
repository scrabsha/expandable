#! /usr/bin/env -S cargo -Zscript

//! `path` shall only be followed by `=>`, `,`, `=`, `|`, `;`, `:`, `>`, `>>`,
//! `[`, `{`, `as`, `where`, or a metavariable with the block fragment
//! specifier kind.
//!
//! https://spec.ferrocene.dev/macros.html#fls_0j7vov4ewfey

#![allow(unused)]

macro_rules! test {
    ($_:path +) => {};
}

fn main() {}
