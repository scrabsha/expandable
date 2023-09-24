#! /usr/bin/env -S cargo -Zscript

//! If the macro repetition has a separator, the shall be allowed by the
//! `MacroRepetitionMatchContent`'s fragment specifier restrictions.
//!
//! https://spec.ferrocene.dev/macros.html#fls_sm4qvshkyly2

#![allow(unused)]

macro_rules! test {
    ($($e:expr)-*) => {};
}

fn main() {}
