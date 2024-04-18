//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>rust-grammar-dpdfa</code></h1>
//! Rust as a  Deterministic PushDown Automaton.
//! </div>
//!
//! <br>
//!
//! This crate provides the [`RustParser`] type, that describes a state machine
//! that has the following properties:
//! - It ingests tokens one by one,
//! - It is completely generic over its span type (anything that is `Copy +
//!   static` is perfect),
//! - It can be fully compared with other `RustParser` objects.
//!
//! # Usage
//!
//! The [`TokenDescription`] enum represents a token to be consumed by the
//! parser. The following example shows how a simple math expression can be
//! parsed:
//!
//! ```rust
//! use rust_grammar_dpdfa::{RustParser, TokenDescription::*};
//!
//! // "42 + 101 * foo"
//! let tokens = [Literal, Plus, Literal, Star, Ident];
//!
//! let mut parser = RustParser::expr();
//!
//! for (span, token) in tokens.into_iter().enumerate() {
//!     parser.step(token, span).expect("Parsing failed");
//! }
//!
//! parser.finish().expect("Parsing failed");
//! ```
//!
//! # Three tokens late
//!
//! In order to be able to peek tokens, the parser does not perform any action
//! for the two first tokens that it `step`s on. Instead, it keeps them in
//! memory and starts the parsing process once the third token is passed.
//!
//! This has two consequences:
//! - [`RustParser::finish`] must be called at the end of input,
//! - In case of error, [`RustParser::step`] and [`RustParser::finish`] return
//!   the span at which the error occured.

mod generated;
#[cfg(test)]
#[macro_use]
mod quote;
#[cfg(test)]
mod tests;

pub use generated::{RustParser, TokenDescription, TransitionData as Transition};
