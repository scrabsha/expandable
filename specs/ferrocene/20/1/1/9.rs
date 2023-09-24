#! /usr/bin/env -S cargo -Zscript

//! `pat_param` shall only be followed by `=>`, `,`, `=`, `|`, `if`, or `in`.
//!
//! https://spec.ferrocene.dev/macros.html#fls_80compimu2gx

#![allow(unused)]

macro_rules! test {
    ($_:pat_param +) => {};
}

fn main() {}
