#! /usr/bin/env -S cargo -Zscript

//! A metavariable indication shall be used in a macro repetition in
//! transcription of the same nesting depth as its corresponding metavariable
//! appears in the macro matcher.
//!
//! https://spec.ferrocene.dev/macros.html#fls_y4podc7ee8lf

#![allow(unused)]

macro_rules! test {
    ($($a:ident)*) => {
        $a
    };
}

fn main() {}
