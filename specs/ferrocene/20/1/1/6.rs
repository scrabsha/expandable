#! /usr/bin/env -S cargo -Zscript

//! `expr` shall only be followed by `=>`, `,`, or `;`
//!
//! https://spec.ferrocene.dev/macros.html#fls_pxr9vnhsafni

#![allow(unused)]

macro_rules! test {
    ($_:expr +) => {};
}

fn main() {}
