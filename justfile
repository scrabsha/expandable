#!/usr/bin/env just --justfile

test:
    cargo test --workspace

fmt:
    cargo fmt
    find tests/ -name "*.rs" -exec rustfmt {} +
    rustfmt rust-grammar-dpdfa/grammar.rs

msrv:
    cargo +1.70.0 test --workspace

readme:
    cargo rustdoc -- --output-format json -Zunstable-options
    cat target/doc/expandable.json | jq ".index[.root].docs" -r > README.md

check-readme:
    cargo rustdoc -- --output-format json -Zunstable-options
    cat target/doc/expandable.json | jq ".index[.root].docs" -r | cmp README.md -

grammar:
    cargo run -p grammar-gen -- rust-grammar-dpdfa/grammar.rs rust-grammar-dpdfa/src/generated.rs
