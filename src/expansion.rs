use std::{cell::RefCell, collections::HashMap};

use crate::{
    expansion,
    grammar::State,
    matcher::{self, Matcher},
    states::StateSet,
    substitution::{self, TokenTree},
    FragmentKind, RepetitionQuantifier, Terminal,
};

type Result<T> = std::result::Result<T, ()>;
type Cursor<'ast> = &'ast [TokenTree];

pub(crate) fn check_arm(bindings: Matcher, substitution: &[expansion::TokenTree]) -> Result<()> {
    let bindings = bindings.bindings;
    ExpCtx::check_rule(bindings, substitution)
}

#[derive(Default)]
struct ExpCtx<'ast> {
    parse_single_tree: RefCell<HashMap<(StateSet, &'ast TokenTree), Result<StateSet>>>,
    // todo: we also want to see which depth a macro repeats at.
    bindings: HashMap<String, FragmentKind>,
}

impl<'ast> ExpCtx<'ast> {
    fn check_rule(bindings: HashMap<String, FragmentKind>, subst: &[TokenTree]) -> Result<()> {
        let ctxt = ExpCtx::new(bindings);
        ctxt.parse_stream(StateSet::singleton(State::A), subst)
            .map(drop)
    }

    fn new(bindings: HashMap<String, FragmentKind>) -> ExpCtx<'ast> {
        ExpCtx {
            bindings,
            ..Default::default()
        }
    }

    fn parse_single_tree(&self, states: StateSet, tree: &'ast TokenTree) -> Result<StateSet> {
        {
            let table = self.parse_single_tree.borrow();
            if let Some(rslt) = table.get(&(states, tree)) {
                return rslt.clone();
            }
        }
        let rslt = match tree {
            TokenTree::Repetition {
                inner,
                separator,
                quantifier,
            } => self.parse_repetition(
                states,
                inner,
                separator.as_ref().map(|b| b.as_ref()),
                *quantifier,
            ),

            other => Ok(states
                .into_iter()
                .map(|state| self.parse_single_tree_inner(state, other))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .flatten()
                .collect()),
        };

        {
            let mut table = self.parse_single_tree.borrow_mut();
            table.insert((states, tree), rslt.clone());
        }

        rslt
    }

    fn parse_single_tree_inner(&self, state: State, tree: &'ast TokenTree) -> Result<StateSet> {
        Ok(dbg!(match (state, tree) {
            (_, TokenTree::Repetition { .. }) => {
                unreachable!("Repetitions should be handled by `parse_single_tree`")
            }

            (State::A, TokenTree::Terminal(Terminal::Ident(_))) => StateSet::singleton(State::B),

            (State::A, TokenTree::Parenthesed(inner)) => {
                if !self
                    .parse_stream(StateSet::singleton(State::A), &inner)?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(());
                };

                StateSet::singleton(State::B)
            }

            (State::A, _) => return Err(()),

            (State::B, TokenTree::Terminal(Terminal::Plus | Terminal::Times)) => {
                StateSet::singleton(State::A)
            }

            (State::B, _) => return Err(()),
        }))
    }

    fn parse_stream(&self, mut states: StateSet, stream: Cursor<'ast>) -> Result<StateSet> {
        for tree in stream {
            states = self.parse_single_tree(states, tree)?;
        }

        if states.into_iter().any(|state| !state.is_accepting()) {
            return Err(());
        }

        Ok(states)
    }

    fn parse_repetition(
        &self,
        states: StateSet,
        stream: Cursor<'ast>,
        sep: Option<&'ast TokenTree>,
        quantifier: RepetitionQuantifier,
    ) -> Result<StateSet> {
        match quantifier {
            RepetitionQuantifier::ZeroOrOne => {
                assert!(sep.is_none());
                self.parse_zero_or_one_repetitions(states, stream)
            }

            RepetitionQuantifier::ZeroOrMore => {
                self.parse_one_or_more_repetitions(states, stream, sep)
            }

            RepetitionQuantifier::OneOrMore => {
                self.parse_zero_or_more_repetitions(states, stream, sep)
            }
        }
    }

    fn parse_zero_or_one_repetitions(
        &self,
        states: StateSet,
        stream: Cursor<'ast>,
    ) -> Result<StateSet> {
        let candidates = states.clone();
        let candidates = candidates.union(self.parse_single_repetition(states, None, stream)?);

        Ok(candidates)
    }

    fn parse_zero_or_more_repetitions(
        &self,
        states: StateSet,
        stream: Cursor<'ast>,
        sep: Option<&'ast TokenTree>,
    ) -> Result<StateSet> {
        self.parse_zero_or_more_repetitions_inner(states, stream, sep, true)
    }

    fn parse_zero_or_more_repetitions_inner(
        &self,
        states: StateSet,
        stream: Cursor<'ast>,
        sep: Option<&'ast TokenTree>,
        mut first: bool,
    ) -> Result<StateSet> {
        let mut outcomes = StateSet::empty();
        let mut to_test = states;

        while !to_test.is_included_in(outcomes) {
            outcomes = outcomes.union(to_test);

            let sep = if !first { sep } else { None };
            to_test = self.parse_single_repetition(to_test, sep, stream)?;
            first = false;
        }

        Ok(outcomes)
    }

    fn parse_one_or_more_repetitions(
        &self,
        states: StateSet,
        stream: Cursor<'ast>,
        sep: Option<&'ast TokenTree>,
    ) -> Result<StateSet> {
        let states = self.parse_single_repetition(states, None, stream)?;

        self.parse_zero_or_more_repetitions_inner(states, stream, sep, false)
    }

    fn parse_single_repetition(
        &self,
        states: StateSet,
        sep: Option<&'ast TokenTree>,
        stream: Cursor<'ast>,
    ) -> Result<StateSet> {
        let states = match sep {
            Some(sep) => self.parse_single_tree(states, sep)?,
            None => states,
        };

        self.parse_stream(states, stream)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_valid_arm {
        ( ( $( $matcher:tt )* ) => { $( $substitution:tt )* }) => {};
    }
}
