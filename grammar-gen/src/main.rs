use std::{env, fs};

mod codegen;
mod fmt;
mod mir;
mod opt;
mod parse;

fn main() {
    let inpath = env::args().nth(1).expect("missing path to grammar file");
    let content = fs::read_to_string(inpath).expect("failed to read grammar file");
    let grammar = match parse::parse(&content) {
        Ok(grammar) => grammar,
        Err(err) => {
            panic!(
                "failed to parse grammar file at {}:{}: {}",
                err.span().start().line,
                err.span().start().column,
                err
            );
        }
    };

    let productions = mir::productions_from_document(grammar);
    let productions = opt::optimize_grammar(productions);

    let tokens = codegen::generate_code(productions);

    let raw_output_str = tokens.to_string();
    let formatted_output = fmt::fmt(&raw_output_str);

    let outpath = env::args().nth(2).expect("missing path to output file");
    fs::write(outpath, formatted_output).expect("failed to write output file");
}
