#!/usr/bin/env just --justfile

test:
    cargo test --workspace

fmt:
    cargo fmt
    find tests/ -name "*.rs" -exec rustfmt {} +