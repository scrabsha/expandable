use tinyset::Fits64;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct DynamicState {
    pub(crate) state: State,
    // This is probably the most disappointing part of the codebase.
    //
    // The Rust grammar is definitely not regular. It should be parsed with
    // pushdown automaton. We circumvent this issue by manipulating trees
    // instead of raw sequence of tokens. This approach works well, as long as
    // we can guess in advance if a token is a delimiter or not.
    //
    // Depending on the situation, `<` and `>` may or may not be considered as
    // delimiters. For instance, `<` and `>` are delimiters in
    // `Iterator<Item = u8>`, but are definitely not delimiters in
    // `4 < x && 6 > y`. As a result, we do have to use a pushdown automaton in
    // order to parse the Rust syntax.
    //
    // Luckily for us, there is only one symbol that is pushed and popped in the
    // state machine's stack, which represent the amount of "delimiter `<`" that
    // have not been closed by a "delimiter `>`". This means that this entire
    // stack can be represented with an integer.
    pub(crate) opened_lts: u8,
}

impl DynamicState {
    pub(crate) const fn with_state(self, state: State) -> DynamicState {
        DynamicState { state, ..self }
    }

    pub(crate) fn is_accepting(self) -> bool {
        self.state.is_accepting() && self.opened_lts == 0
    }

    pub(crate) const fn increment_to(self, state: State) -> DynamicState {
        let opened_lts = self.opened_lts + 1;

        DynamicState { state, opened_lts }
    }

    pub(crate) const fn decrement_to(self, state: State) -> Option<DynamicState> {
        // HACK: once `?` is legal in const context, replace this `match` with
        // a `?` thing.
        let opened_lts = match self.opened_lts.checked_sub(1) {
            Some(v) => v,
            None => return None,
        };

        Some(DynamicState { state, opened_lts })
    }
}

impl Fits64 for DynamicState {
    unsafe fn from_u64(x: u64) -> DynamicState {
        let [a, b, c, ..] = u64::to_ne_bytes(x);

        DynamicState {
            opened_lts: a,
            state: u16::from_ne_bytes([b, c]).into(),
        }
    }

    fn to_u64(self) -> u64 {
        let a = self.opened_lts;
        let [b, c] = u16::to_ne_bytes(self.state as u16);
        u64::from_ne_bytes([a, b, c, 0, 0, 0, 0, 0])
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum State {
    ExprStart,
    AfterBinop,

    ItemStart,
    AfterFnKw,
    AfterFnName,

    FnParamStart,

    AfterFnParam,
}

impl State {
    pub(crate) fn is_accepting(self) -> bool {
        use State::*;

        const ACCEPTING_STATES: [State; 3] = [AfterBinop, ItemStart, FnParamStart];

        ACCEPTING_STATES.contains(&self)
    }

    pub(crate) fn into_dynamic_state(self) -> DynamicState {
        DynamicState {
            state: self,
            opened_lts: 0,
        }
    }
}

impl From<u16> for State {
    fn from(value: u16) -> State {
        use State::*;
        const VALUES: [State; 7] = [
            ExprStart,
            AfterBinop,
            ItemStart,
            AfterFnKw,
            AfterFnName,
            FnParamStart,
            AfterFnParam,
        ];

        VALUES[value as usize]
    }
}
