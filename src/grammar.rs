#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum State {
    A,
    B,
}

impl State {
    pub(crate) fn is_accepting(self) -> bool {
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
