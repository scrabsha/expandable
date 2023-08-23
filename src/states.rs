use crate::grammar::State;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct StateSet(
    // TODO: write a proper impl
    u8,
);

impl StateSet {
    pub(crate) fn empty() -> StateSet {
        StateSet(0)
    }

    fn set(mut self, val: State) -> StateSet {
        let offset = val as u8;
        self.0 |= 1 << offset;
        self
    }

    pub(crate) fn singleton(val: State) -> StateSet {
        StateSet::empty().set(val)
    }

    pub(crate) fn is_included_in(self, rhs: StateSet) -> bool {
        rhs.0 | self.0 == rhs.0
    }

    pub(crate) fn union(self, rhs: StateSet) -> StateSet {
        StateSet(self.0 | rhs.0)
    }
}

impl IntoIterator for StateSet {
    type Item = State;
    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { set: self, cur: 0 }
    }
}

impl FromIterator<State> for StateSet {
    fn from_iter<T: IntoIterator<Item = State>>(iter: T) -> Self {
        let mut set = StateSet::empty();

        for i in iter {
            set = set.set(i);
        }

        set
    }
}

pub(crate) struct IntoIter {
    set: StateSet,
    cur: u8,
}

impl Iterator for IntoIter {
    type Item = State;

    fn next(&mut self) -> Option<State> {
        for i in self.cur..8 {
            if (self.set.0 & 1 << i) != 0 {
                self.cur = i + 1;
                return Some(State::try_from(i).unwrap());
            }
        }

        None
    }
}
