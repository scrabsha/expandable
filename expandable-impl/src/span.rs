use std::cell::Cell;
use std::fmt::{Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) struct DebugSpan(pub(crate) usize);

impl Debug for DebugSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub(crate) struct DebugSpanBuilder {
    counter: Cell<usize>,
}

impl DebugSpanBuilder {
    pub(crate) fn new() -> DebugSpanBuilder {
        DebugSpanBuilder {
            counter: Cell::new(0),
        }
    }

    pub(crate) fn mk_span(&self) -> DebugSpan {
        let id = self.counter.get();
        self.counter.set(id + 1);
        DebugSpan(id)
    }
}
