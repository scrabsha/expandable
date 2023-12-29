use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Token {
    ColonColon,
    LessThan,
    GreaterThan,
    Comma,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            // Token::ColonColon => write!(f, "::"),
            Token::ColonColon => write!(f, "coloncolon"),
            // Token::LessThan => write!(f, "<"),
            Token::LessThan => write!(f, "lessthan"),
            // Token::GreaterThan => write!(f, ">"),
            Token::GreaterThan => write!(f, "greaterthan"),
            // Token::Comma => write!(f, ","),
            Token::Comma => write!(f, "comma"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Fragment {
    Ident,
    Literal,
    Lifetime,
}

impl Display for Fragment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Fragment::Ident => write!(f, "ident"),
            Fragment::Literal => write!(f, "literal"),
            Fragment::Lifetime => write!(f, "lifetime"),
        }
    }
}