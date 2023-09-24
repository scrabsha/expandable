#! /usr/bin/env -S cargo -Zscript

//! `vis` shall only be followed by `,`, an identifier except for `priv`, any
//! token that may begin a `TypeSpecification`, or a metavariable with the
//! `ident`, `ty` or `block` fragment specifier kind.
//!
//! https://spec.ferrocene.dev/macros.html#fls_nbbygzwuxjfp

#![allow(unused)]

macro_rules! test {
    ($_:ty +) => {};
}

fn main() {}
