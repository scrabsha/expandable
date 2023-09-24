#! /usr/bin/env -S cargo -Zscript

//! `pat` shall only be followed by `=>`, `,`, `=`, `|`, `if`, or `in`.
//!
//! https://spec.ferrocene.dev/macros.html#fls_epyotejj11n0

#![allow(unused)]

macro_rules! test {
    ($_:pat +) => {};
}

fn main() {}
