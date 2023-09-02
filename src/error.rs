use crate::{grammar::TokenDescription, Terminal};

/// An error that is generated when checking an incorrect macro.
///
/// This enum allows crate users to handle and report errors detected by
/// [`check_macro`].
///
/// Everything in this enum in marked as `non_exhaustive`, in order to partially
/// mitigate future variant additions.
///
/// [`check_macro`]: crate::check_macro
#[derive(Debug)]
#[non_exhaustive]
pub enum Error<Span> {
    /// Generated when the macro definition itself doesn't parse correctly.
    ///
    /// The Rust compiler is likely to emit an error anyway. Below is an
    /// example of code that triggers this error:
    ///
    /// ```rust,compile_fail
    /// macro_rules! blatant_error {
    ///     () => =>;
    ///     //    |
    ///     //    error: macro rhs must be delimited.
    /// }
    /// ```
    ///
    /// This prevents us from doing any analyses.
    #[non_exhaustive]
    ParsingFailed {
        /// What was expected.
        what: Vec<MacroRuleNode>,
        /// Where it was expected.
        where_: Span,
    },

    /// An EOF was reached when it was not expected.
    #[non_exhaustive]
    UnexpectedEnd {
        /// The position the parser was at when it reached EOF.
        last_token: Option<Span>,
    },

    /// The macro may expand to invalid AST.
    ///
    /// This variant is very minimal for now. We may add more information to
    /// it in the future. Please open an issue if you have opinions on what
    /// to add!
    #[non_exhaustive]
    InvalidProducedAst {
        /// Where the error happens.
        span: Span,
        /// What tokens are expected here.
        expected: &'static [TokenDescription],
    },

    /// A macro expansion refers to a metavariable that is not defined in the
    /// macro match arm.
    ///
    /// If you ever hit this error on a macro that works, then please file an
    /// issue.
    #[non_exhaustive]
    UnboundMetavariable {
        /// The name of the metavariable that was used.
        name: String,
        /// Where it was used.
        where_: Span,
    },
}

/// Various nodes that can be expected in a `macro_rules!` invocation.
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum MacroRuleNode {
    /// A matcher (everything that comes _before_ the `=>` of a macro rule.
    Matcher,
    /// A transcriber (everything that comes _after_ the `=>` of a macro rule.
    Transcriber,
    /// A repetition.
    Repetition,
    /// A fragment name.
    FragmentName,
    /// A fragment type specifier (`ident`, `expr`, ...).
    FragmentSpecifier,
    /// A meta variable match, such as `$a:ident`.
    MetaVariableMatch,
    /// A repetition quantifier (`?`, `*`, `+`).
    RepetitionQuantifier,
    /// A repetition separator (the `,` in `$( $expr ),*.
    RepetitionSeparator,
    /// Any terminal.
    Terminal(Terminal),
}
