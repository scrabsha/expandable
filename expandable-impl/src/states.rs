// Architectural invariant: this module contains types that allow us to handle
// multiple parsing states at once.

use std::collections::HashSet;

use crate::grammar::DynamicState;

// Let's pretend this is fast enough and use clone when required.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct DynamicStateSet(HashSet<DynamicState>);

impl DynamicStateSet {
    pub(crate) fn empty() -> DynamicStateSet {
        DynamicStateSet(HashSet::new())
    }

    fn set(mut self, state: DynamicState) -> DynamicStateSet {
        self.0.insert(state);
        self
    }

    pub(crate) fn singleton(state: DynamicState) -> DynamicStateSet {
        DynamicStateSet::empty().set(state)
    }

    pub(crate) fn is_included_in(&self, rhs: &DynamicStateSet) -> bool {
        (&self.0 - &rhs.0).is_empty()
    }

    pub(crate) fn union(self, rhs: DynamicStateSet) -> DynamicStateSet {
        DynamicStateSet(&self.0 | &rhs.0)
    }
}

impl IntoIterator for DynamicStateSet {
    type IntoIter = IntoIter;
    type Item = DynamicState;

    fn into_iter(self) -> IntoIter {
        IntoIter(self.0.into_iter())
    }
}

impl FromIterator<DynamicState> for DynamicStateSet {
    fn from_iter<T: IntoIterator<Item = DynamicState>>(iter: T) -> Self {
        let mut set = DynamicStateSet::empty();

        for i in iter {
            set = set.set(i);
        }

        set
    }
}

pub(crate) struct IntoIter(std::collections::hash_set::IntoIter<DynamicState>);

impl Iterator for IntoIter {
    type Item = DynamicState;

    fn next(&mut self) -> Option<DynamicState> {
        self.0.next()
    }
}
