// Architectural invariant: this module contains a development-only span type
// aiming to ease debugging. This module is cfg-ed out when the crate is used
// as a dependency.

use std::fmt::{Debug, Formatter};

#[derive(Copy, Clone)]
pub struct DebugSpan(pub(crate) usize);

#[cfg(test)]
impl DebugSpan {
    pub(crate) const EOF: DebugSpan = DebugSpan(42101);
}

impl Debug for DebugSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct DebugSpanBuilder {
    counter: usize,
}

impl DebugSpanBuilder {
    pub fn new() -> DebugSpanBuilder {
        DebugSpanBuilder::default()
    }

    pub fn mk_span(&mut self) -> DebugSpan {
        let id = self.counter;
        self.counter += 1;
        DebugSpan(id)
    }
}
