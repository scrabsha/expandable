#!/usr/bin/env just --justfile

test:
    cargo test --workspace

fmt:
    cargo fmt
    find tests/ -name "*.rs" -exec rustfmt {} +

msrv:
    cargo +1.65.0 test --workspace

readme:
    cargo rustdoc -- --output-format json -Zunstable-options
    cat target/doc/expandable.json | jq ".index[.root].docs" -r > README.md

check-readme:
    cargo rustdoc -- --output-format json -Zunstable-options
    cat target/doc/expandable.json | jq ".index[.root].docs" -r | cmp README.md -