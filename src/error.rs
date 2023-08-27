use crate::Terminal;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error<Span> {
    #[non_exhaustive]
    ParsingFailed {
        what: Vec<MacroRuleNode>,
        where_: Span,
    },

    #[non_exhaustive]
    UnexpectedEnd { last_token: Option<Span> },

    // TODO: THIS IS NOT THE REPORTING THAT WE WANT TO SHIP
    // figure out a clean way to produce expansion errors.
    #[non_exhaustive]
    InvalidProducedAst,

    #[non_exhaustive]
    UnboundMetaVariable { name: String, where_: Span },

    // TODO: this is not the error we want to ship
    #[non_exhaustive]
    InvalidMetaVariableContext { name: String },
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum MacroRuleNode {
    Matcher,
    Transcriber,
    Repetition,
    FragmentName,
    FragmentSpecifier,
    MetaVariableMatch,
    RepetitionQuantifier,
    RepetitionSeparator,
    Terminal(Terminal),
}
