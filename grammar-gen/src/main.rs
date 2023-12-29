use std::fs;

use anyhow::{Context, Result};

mod common;
mod grammar_maker;
mod spec_parser;

mod greibach_normal_form;
mod chomsky;

fn main() -> Result<()> {
    let path = "grammar-gen/specs.txt";
    let spec = fs::read_to_string(path).context("Failed to read spec file")?;

    let specs = spec_parser::parse_specs(&spec)?;
    let grammar = grammar_maker::build_from_specs(specs);
    println!("{}", grammar);

    Ok(())
}
