mod matcher;

use std::{cell::RefCell, collections::HashMap};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[must_use]
struct StateSet(
    // TODO: write a proper bitset impl - it's ok for now, as we only have two
    // states.
    u8,
);

impl StateSet {
    fn empty() -> StateSet {
        StateSet(0)
    }

    fn set(mut self, val: State) -> StateSet {
        let offset = val as u8;
        self.0 |= 1 << offset;
        self
    }

    fn singleton(val: State) -> StateSet {
        let mut this = StateSet::empty();
        this.set(val)
    }

    fn is_included_in(self, rhs: StateSet) -> bool {
        rhs.0 | self.0 == rhs.0
    }

    fn union(self, rhs: StateSet) -> StateSet {
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

struct IntoIter {
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

type Result<T> = std::result::Result<T, ()>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Query<'ast> {
    pos: Cursor<'ast>,
    state: State,
}

type QueryResult<'ast> = Vec<(State, Cursor<'ast>)>;

type Cursor<'ast> = &'ast [TokenTree];

#[derive(Default)]
struct ExpCtx<'ast> {
    parse_single_tree: RefCell<HashMap<(StateSet, &'ast TokenTree), Result<StateSet>>>,
    // todo: we also want to see which depth a macro repeats at.
    bindings: HashMap<String, FragmentKind>,
}

impl<'ast> ExpCtx<'ast> {
    pub fn check_rule(matcher: &[TokenTree], subst: &[TokenTree]) -> Result<()> {
        let ctxt = ExpCtx::new();
        ctxt.parse_stream(StateSet::singleton(State::A), subst)
            .map(drop)
    }

    fn new() -> ExpCtx<'ast> {
        ExpCtx::default()
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

            (State::A, TokenTree::Terminal(Terminal::Ident)) => StateSet::singleton(State::B),

            (State::A, TokenTree::Parenthesed(inner)) => {
                if !self
                    .parse_stream(StateSet::singleton(State::A), inner)?
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum State {
    A,
    B,
}

impl State {
    fn is_accepting(self) -> bool {
        self == State::B
    }
}

impl TryFrom<u8> for State {
    type Error = ();

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        const VALUES: [State; 2] = [State::A, State::B];

        VALUES.get(value as usize).copied().ok_or(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum TokenTree {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree>),
    Repetition {
        inner: Vec<TokenTree>,
        separator: Option<Box<TokenTree>>,
        quantifier: RepetitionQuantifier,
    },
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Terminal {
    Ident,
    Plus,
    Times,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum FragmentKind {
    Ident,
    Expr,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum RepetitionQuantifier {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}

macro_rules! token_tree {
    ( @inner, ( $( $tt:tt )* ) ) => {
        TokenTree::Parenthesed(token_tree!( $( $tt )* ))
    };

    ( @inner, $id:ident) => {
        TokenTree::Terminal(Terminal::Ident)
    };

    ( @inner, +) => {
        TokenTree::Terminal(Terminal::Plus)
    };

    ( @inner, *) => {
        TokenTree::Terminal(Terminal::Times)
    };

    ( $( $tt:tt )* ) => {
        vec![
            $(
                token_tree!(@inner, $tt)
            ),*
        ]
    };
}

