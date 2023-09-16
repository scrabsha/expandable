use std::collections::HashMap;

use crate::{
    error::Error,
    grammar::{DynamicState, State},
    matcher::{BindingData, Matcher},
    states::DynamicStateSet,
    substitution::{TokenTree, TokenTreeKind},
    RepetitionQuantifier, RepetitionQuantifierKind,
};

type Cursor<'ast, Span> = &'ast [TokenTree<Span>];

pub(crate) fn check_arm<Span>(
    init_state: State,
    bindings: Matcher<Span>,
    substitution: &[TokenTree<Span>],
) -> Result<(), Error<Span>>
where
    Span: Copy,
{
    let bindings = bindings.bindings;
    ExpCtx::check_rule(bindings, substitution, init_state.into_dynamic_state())
}

struct ExpCtx<Span> {
    // todo: we also want to see which depth a macro repeats at.
    bindings: HashMap<String, BindingData<Span>>,
}

impl<Span> ExpCtx<Span>
where
    Span: Copy,
{
    fn check_rule(
        bindings: HashMap<String, BindingData<Span>>,
        subst: &[TokenTree<Span>],
        initial_state: DynamicState,
    ) -> Result<(), Error<Span>> {
        let ctx = ExpCtx::new(bindings);
        let states = ctx.parse_stream(DynamicStateSet::singleton(initial_state), subst)?;

        if states.into_iter().any(|state| !state.is_accepting()) {
            // TODO: this does not feel like it's the right error kind.
            //
            // As a matter of facts, rustc emits the following message:
            // > macro expansion ends with an incomplete expression: expected expression
            // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=19fb5e7169f37e967ad84e155fe92e22
            return Err(Error::UnexpectedEnd { last_token: None });
        }

        Ok(())
    }

    fn new(bindings: HashMap<String, BindingData<Span>>) -> ExpCtx<Span> {
        ExpCtx { bindings }
    }

    fn parse_single_tree(
        &self,
        states: DynamicStateSet,
        tree: &TokenTree<Span>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        match &tree.kind {
            TokenTreeKind::Repetition {
                inner,
                separator,
                quantifier,
            } => self.parse_repetition(
                states,
                inner,
                separator.as_ref().map(|b| b.as_ref()),
                *quantifier,
            ),

            _ => Ok(states
                .into_iter()
                .map(|state| self.parse_single_tree_inner(state, tree))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect()),
        }
    }

    fn parse_single_tree_inner(
        &self,
        state: DynamicState,
        tree: &TokenTree<Span>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        Ok(match &tree.kind {
            TokenTreeKind::Terminal(t) => {
                DynamicStateSet::singleton(state.accept_terminal(t).map_err(|expected| {
                    Error::InvalidProducedAst {
                        span: tree.span,
                        expected,
                    }
                })?)
            }

            TokenTreeKind::Parenthesed(i) => {
                let (inner_state, next_state) =
                    state
                        .accept_paren()
                        .map_err(|expected| Error::InvalidProducedAst {
                            span: tree.span,
                            expected,
                        })?;

                self.check_delimited_stream(i, inner_state)?;

                DynamicStateSet::singleton(next_state)
            }

            TokenTreeKind::CurlyBraced(i) => {
                let (inner_state, next_state) =
                    state
                        .accept_curly()
                        .map_err(|expected| Error::InvalidProducedAst {
                            span: tree.span,
                            expected,
                        })?;

                self.check_delimited_stream(i, inner_state)?;

                DynamicStateSet::singleton(next_state)
            }

            TokenTreeKind::Fragment(f) => {
                // TODO: return a proper error
                let kind = self.bindings.get(f).expect("Fragment not found").kind;
                DynamicStateSet::singleton(
                    state
                        .accept_fragment(kind)
                        .expect("Fragment kind not accepted"),
                )
            }
            TokenTreeKind::Repetition { .. } => {
                unreachable!("Repetitions should be handled by ExpCtx::parse_single_tree`")
            }
        })
    }

    fn check_delimited_stream(
        &self,
        stream: &[TokenTree<Span>],
        inner_state: DynamicState,
    ) -> Result<(), Error<Span>> {
        if self
            .parse_stream(DynamicStateSet::singleton(inner_state), stream)?
            .into_iter()
            .any(|state| !state.is_accepting())
        {
            return Err(Error::UnexpectedEnd {
                last_token: stream.last().map(|tree| tree.span),
            });
        }

        Ok(())
    }

    fn parse_stream(
        &self,
        mut states: DynamicStateSet,
        stream: Cursor<Span>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        for tree in stream {
            states = self.parse_single_tree(states, tree)?;
        }

        Ok(states)
    }

    fn parse_repetition(
        &self,
        states: DynamicStateSet,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
        quantifier: RepetitionQuantifier<Span>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        match quantifier.kind {
            RepetitionQuantifierKind::ZeroOrOne => {
                assert!(sep.is_none());
                self.parse_zero_or_one_repetitions(states, stream)
            }

            RepetitionQuantifierKind::ZeroOrMore => {
                self.parse_zero_or_more_repetitions(states, stream, sep)
            }

            RepetitionQuantifierKind::OneOrMore => {
                self.parse_one_or_more_repetitions(states, stream, sep)
            }
        }
    }

    fn parse_zero_or_one_repetitions(
        &self,
        states: DynamicStateSet,
        stream: Cursor<Span>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        let candidates = states.clone();
        let candidates = candidates.union(self.parse_single_repetition(states, None, stream)?);

        Ok(candidates)
    }

    fn parse_zero_or_more_repetitions(
        &self,
        states: DynamicStateSet,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        self.parse_zero_or_more_repetitions_inner(states, stream, sep, true)
    }

    fn parse_zero_or_more_repetitions_inner(
        &self,
        states: DynamicStateSet,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
        mut first: bool,
    ) -> Result<DynamicStateSet, Error<Span>> {
        let mut outcomes = DynamicStateSet::empty();
        let mut to_test = states;

        while !to_test.is_included_in(&outcomes) {
            outcomes = outcomes.union(to_test.clone());

            let sep = if !first { sep } else { None };
            to_test = self.parse_single_repetition(to_test, sep, stream)?;
            first = false;
        }

        Ok(outcomes)
    }

    fn parse_one_or_more_repetitions(
        &self,
        states: DynamicStateSet,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
    ) -> Result<DynamicStateSet, Error<Span>> {
        let states = self.parse_single_repetition(states, None, stream)?;

        self.parse_zero_or_more_repetitions_inner(states, stream, sep, false)
    }

    fn parse_single_repetition(
        &self,
        states: DynamicStateSet,
        sep: Option<&TokenTree<Span>>,
        stream: Cursor<Span>,
    ) -> Result<DynamicStateSet, Error<Span>> {
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

    macro_rules! assert_valid_arm {
        ( $test_name:ident { #[$kind:ident]( $( $matcher:tt )* ) => { $( $substitution:tt )* } $(;)? }) => {
            #[test]
            fn $test_name() {
                let matcher: Vec<crate::TokenTree<()>> = quote! { $( $matcher )* };
                let matcher = crate::matcher::TokenTree::from_generic(matcher).expect("Failed to generate `matcher::TokenTree`");
                let bindings = crate::matcher::Matcher::from_generic(&matcher).expect("Failed to generate `matcher::Bindings`");

                let subst = quote! { $( $substitution )* };
                let subst = crate::substitution::TokenTree::from_generic(subst).expect("Failed to generate `substitution::TokenTree`");

                let state = stringify!($kind).parse::<crate::InvocationContext>().expect("Failed to generate `FragmentKind`");
                let state = state.to_state();

                check_arm(state, bindings, &subst).expect("Checking failed");
            }
        }
    }

    assert_valid_arm! {
        simplest_expression {
            #[expr]
            () => { a }
        }
    }

    assert_valid_arm! {
        maths {
            #[expr]
            () => { a * b + c}
        }
    }

    assert_valid_arm! {
        with_fragment {
            #[expr]
            ( @a:ident ) => { @a }
        }
    }

    assert_valid_arm! {
        with_repetition_question {
            #[expr]
            () => { a @( * b)? }
        }
    }

    assert_valid_arm! {
        with_repetition_plus {
            #[expr]
            ( @a:ident ) => { a @( * )b+ c }
        }
    }

    assert_valid_arm! {
        function_without_fragment {
            #[item]
            () => { fn foo() { a } }
        }
    }

    assert_valid_arm! {
        function_with_custom_name {
            #[item]
            (@name:ident) => { fn @name() { a } }
        }
    }

    assert_valid_arm! {
        custom_function {
            #[item]
            (
                fn @name:ident() { @body:expr }
            ) => {
                fn @name() { @body }
            }
        }
    }
}
