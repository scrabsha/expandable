// Architectural invariant: this module contains a development-only span type
// aiming to ease debugging. This module is cfg-ed out when the crate is used
// as a dependency.

use std::fmt::{Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) struct DebugSpan(pub(crate) usize);

impl Debug for DebugSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub(crate) struct DebugSpanBuilder {
    counter: usize,
}

impl DebugSpanBuilder {
    pub(crate) fn new() -> DebugSpanBuilder {
        DebugSpanBuilder { counter: 0 }
    }

    pub(crate) fn mk_span(&mut self) -> DebugSpan {
        let id = self.counter;
        self.counter += 1;
        DebugSpan(id)
    }
}
