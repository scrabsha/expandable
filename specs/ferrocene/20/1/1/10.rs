#! /usr/bin/env -S cargo -Zscript

//! `stmt` shall only be followed by `=>`, `,`, or `;`.
//!
//! https://spec.ferrocene.dev/macros.html#fls_dfmrwswi8e5z

#![allow(unused)]

macro_rules! test {
    ($_:pat_param +) => {};
}

fn main() {}
