// Architectural invariant: this module contains types that allow us to handle
// multiple parsing states at once.

use std::{
    cmp::Ordering,
    collections::HashSet,
    hash::{Hash, Hasher},
};

use crate::{grammar::DynamicState, FragmentKind, Terminal, TokenDescription};

// Let's pretend this is fast enough and use clone when required.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct DynamicStateSet<'ast>(HashSet<StateAndChoices<'ast>>);

impl<'ast> DynamicStateSet<'ast> {
    pub(crate) fn empty() -> DynamicStateSet<'ast> {
        DynamicStateSet(HashSet::new())
    }

    fn set(mut self, state: StateAndChoices<'ast>) -> DynamicStateSet<'ast> {
        match self.0.take(&state) {
            Some(existing) => {
                let merged = existing.merge(state);
                self.0.insert(merged);
            }
            None => {
                self.0.insert(state);
            }
        }

        self
    }

    pub(crate) fn singleton(state: StateAndChoices) -> DynamicStateSet {
        DynamicStateSet::empty().set(state)
    }

    pub(crate) fn is_included_in(&self, rhs: &DynamicStateSet) -> bool {
        (&self.0 - &rhs.0).is_empty()
    }

    pub(crate) fn union(self, rhs: DynamicStateSet<'ast>) -> DynamicStateSet<'ast> {
        DynamicStateSet(&self.0 | &rhs.0)
    }
}

impl<'ast> IntoIterator for DynamicStateSet<'ast> {
    type IntoIter = IntoIter<'ast>;
    type Item = StateAndChoices<'ast>;

    fn into_iter(self) -> IntoIter<'ast> {
        IntoIter(self.0.into_iter())
    }
}

impl<'ast> FromIterator<StateAndChoices<'ast>> for DynamicStateSet<'ast> {
    fn from_iter<T: IntoIterator<Item = StateAndChoices<'ast>>>(iter: T) -> Self {
        let mut set = DynamicStateSet::empty();

        for i in iter {
            set = set.set(i);
        }

        set
    }
}

pub(crate) struct IntoIter<'ast>(std::collections::hash_set::IntoIter<StateAndChoices<'ast>>);

impl<'ast> Iterator for IntoIter<'ast> {
    type Item = StateAndChoices<'ast>;

    fn next(&mut self) -> Option<StateAndChoices<'ast>> {
        self.0.next()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StateAndChoices<'ast> {
    pub state: DynamicState,
    pub tokens: Vec<AccumulatedToken<'ast>>,
}

impl<'ast> StateAndChoices<'ast> {
    pub(crate) fn accept(
        mut self,
        term: impl Into<AccumulatedToken<'ast>>,
        descr: TokenDescription,
    ) -> Result<StateAndChoices<'ast>, Vec<TokenDescription>> {
        self.state.clone().accept(descr).map(|state| {
            self.tokens.push(term.into());
            self.state = state;
            self
        })
    }

    pub(crate) fn accept_fragment(
        self,
        fragment: FragmentKind,
    ) -> Result<StateAndChoices<'ast>, Vec<TokenDescription>> {
        self.state.accept_fragment(fragment).map(|state| {
            let mut tokens = self.tokens;
            tokens.push(fragment.into());
            StateAndChoices { state, tokens }
        })
    }

    pub(crate) fn fresh_stack(&mut self) -> StateAndChoices<'ast> {
        let new_state = self.state.fresh_stack();
        StateAndChoices {
            state: new_state,
            tokens: self.tokens.clone(),
        }
    }

    pub(crate) fn with_old_stack(
        &self,
        old_state: &StateAndChoices<'ast>,
    ) -> StateAndChoices<'ast> {
        StateAndChoices {
            state: self.state.with_old_stack(&old_state.state),
            tokens: {
                let mut tokens = self.tokens.clone();
                tokens.extend(old_state.tokens.iter().cloned());
                tokens
            },
        }
    }

    pub(crate) fn empty(state: DynamicState) -> StateAndChoices<'ast> {
        StateAndChoices {
            state,
            tokens: Vec::new(),
        }
    }

    pub(crate) fn is_accepting(&self) -> bool {
        self.state.is_accepting()
    }

    pub(crate) fn merge(self, rhs: StateAndChoices<'ast>) -> StateAndChoices<'ast> {
        assert_eq!(self.state, rhs.state);

        let state = self.state;

        let tokens = match self.tokens.len().cmp(&rhs.tokens.len()) {
            Ordering::Less => self.tokens,
            Ordering::Equal | Ordering::Greater => rhs.tokens,
        };

        StateAndChoices { state, tokens }
    }
}

impl<'ast> PartialEq for StateAndChoices<'ast> {
    fn eq(&self, rhs: &StateAndChoices<'ast>) -> bool {
        self.state == rhs.state
    }
}

impl<'ast> Eq for StateAndChoices<'ast> {}

impl<'ast> Hash for StateAndChoices<'ast> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.state.hash(state);
    }
}

#[derive(Clone, Debug)]
pub(crate) enum AccumulatedToken<'ast> {
    Terminal(&'ast Terminal),
    // Invariant: must only contain delimiters - nothing else.
    Delimiter(TokenDescription),
    Fragment(FragmentKind),
}

impl<'ast> From<&'ast Terminal> for AccumulatedToken<'ast> {
    fn from(term: &'ast Terminal) -> AccumulatedToken<'ast> {
        AccumulatedToken::Terminal(term)
    }
}

impl<'ast> From<TokenDescription> for AccumulatedToken<'ast> {
    fn from(descr: TokenDescription) -> AccumulatedToken<'ast> {
        AccumulatedToken::Delimiter(descr)
    }
}

impl<'ast> From<FragmentKind> for AccumulatedToken<'ast> {
    fn from(fragment: FragmentKind) -> AccumulatedToken<'ast> {
        AccumulatedToken::Fragment(fragment)
    }
}
