use std::collections::HashMap;

use crate::{
    expansion,
    grammar::{DynamicState, State},
    matcher::Matcher,
    states::DynamicStateSet,
    substitution::TokenTree,
    FragmentKind, RepetitionQuantifier, Terminal,
};

type Result<T> = std::result::Result<T, ()>;
type Cursor<'ast> = &'ast [TokenTree];

pub(crate) fn check_arm(
    ty: FragmentKind,
    bindings: Matcher,
    substitution: &[expansion::TokenTree],
) -> Result<()> {
    let bindings = bindings.bindings;
    ExpCtx::check_rule(bindings, substitution, ty.to_dynamic_state())
}

#[derive(Default)]
struct ExpCtx {
    state_sets: Vec<DynamicStateSet>,
    // todo: we also want to see which depth a macro repeats at.
    bindings: HashMap<String, FragmentKind>,
}

impl ExpCtx {
    fn check_rule(
        bindings: HashMap<String, FragmentKind>,
        subst: &[TokenTree],
        initial_state: DynamicState,
    ) -> Result<()> {
        let ctxt = ExpCtx::new(bindings);
        ctxt.parse_stream(DynamicStateSet::singleton(initial_state), subst)
            .map(drop)
    }

    fn new(bindings: HashMap<String, FragmentKind>) -> ExpCtx {
        ExpCtx {
            bindings,
            ..Default::default()
        }
    }

    fn parse_single_tree(
        &self,
        states: DynamicStateSet,
        tree: &TokenTree,
    ) -> Result<DynamicStateSet> {
        match tree {
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
        }
    }

    fn parse_single_tree_inner(
        &self,
        state: DynamicState,
        tree: &TokenTree,
    ) -> Result<DynamicStateSet> {
        // TODO: we will have to deal with a huge number of transitions. We *have* to
        // find a way to generate this function, perhaps with a macro
        // :upside_down:.
        Ok(match (state.state, tree) {
            (_, TokenTree::Repetition { .. }) => {
                unreachable!("Repetitions should be handled by `parse_single_tree`")
            }

            // fn
            (State::ItemStart, TokenTree::Terminal(Terminal::Ident(fn_))) if fn_ == "fn" => {
                DynamicStateSet::singleton(state.with_state(State::AfterFnKw))
            }

            (State::ItemStart, _) => return Err(()),

            // fn foo
            (State::AfterFnKw, TokenTree::Terminal(Terminal::Ident(_))) => {
                DynamicStateSet::singleton(state.with_state(State::AfterFnName))
            }

            (State::AfterFnKw, TokenTree::Fragment(name)) => {
                let kind = self.bindings.get(name).ok_or(())?;

                if *kind != FragmentKind::Ident {
                    return Err(());
                }

                DynamicStateSet::singleton(state.with_state(State::AfterFnName))
            }

            (State::AfterFnKw, _) => return Err(()),

            // fn foo()
            (State::AfterFnName, TokenTree::Parenthesed(params)) => {
                if self
                    .parse_stream(
                        DynamicStateSet::singleton(state.with_state(State::FnParamStart)),
                        &params,
                    )?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(());
                };

                DynamicStateSet::singleton(state.with_state(State::AfterFnParam))
            }

            // fn foo(
            (State::FnParamStart, _) => return Err(()),

            (State::AfterFnName, _) => return Err(()),

            // fn foo() {}
            (State::AfterFnParam, TokenTree::CurlyBraced(inner)) => {
                if self
                    .parse_stream(
                        DynamicStateSet::singleton(state.with_state(State::ExprStart)),
                        &inner,
                    )?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(());
                }

                DynamicStateSet::singleton(state.with_state(State::ItemStart))
            }

            (State::AfterFnParam, _) => return Err(()),

            (State::ExprStart, TokenTree::Terminal(Terminal::Ident(_))) => {
                DynamicStateSet::singleton(state.with_state(State::AfterBinop))
            }

            (State::ExprStart, TokenTree::Parenthesed(inner)) => {
                if self
                    .parse_stream(
                        DynamicStateSet::singleton(state.with_state(State::ExprStart)),
                        &inner,
                    )?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(());
                };

                DynamicStateSet::singleton(state.with_state(State::AfterBinop))
            }

            (State::ExprStart, TokenTree::Fragment(name)) => {
                let kind = self.bindings.get(name).ok_or(())?;

                match kind {
                    FragmentKind::Ident => {
                        DynamicStateSet::singleton(state.with_state(State::AfterBinop))
                    }
                    FragmentKind::Expr => {
                        DynamicStateSet::singleton(state.with_state(State::AfterBinop))
                    }
                    FragmentKind::Item => return Err(()),
                }
            }

            (State::ExprStart, _) => return Err(()),

            (State::AfterBinop, TokenTree::Terminal(Terminal::Plus | Terminal::Times)) => {
                DynamicStateSet::singleton(state.with_state(State::ExprStart))
            }

            (State::AfterBinop, _) => return Err(()),
        })
    }

    fn parse_stream(&self, mut states: DynamicStateSet, stream: Cursor) -> Result<DynamicStateSet> {
        for tree in stream {
            states = self.parse_single_tree(states, tree)?;
        }

        Ok(states)
    }

    fn parse_repetition(
        &self,
        states: DynamicStateSet,
        stream: Cursor,
        sep: Option<&TokenTree>,
        quantifier: RepetitionQuantifier,
    ) -> Result<DynamicStateSet> {
        match quantifier {
            RepetitionQuantifier::ZeroOrOne => {
                assert!(sep.is_none());
                self.parse_zero_or_one_repetitions(states, stream)
            }

            RepetitionQuantifier::ZeroOrMore => {
                self.parse_one_or_more_repetitions(states, stream, sep)
            }

            RepetitionQuantifier::OneOrMore => {
                self.parse_one_or_more_repetitions(states, stream, sep)
            }
        }
    }

    fn parse_zero_or_one_repetitions(
        &self,
        states: DynamicStateSet,
        stream: Cursor,
    ) -> Result<DynamicStateSet> {
        let candidates = states.clone();
        let candidates = candidates.union(self.parse_single_repetition(states, None, stream)?);

        Ok(candidates)
    }

    fn parse_zero_or_more_repetitions(
        &self,
        states: DynamicStateSet,
        stream: Cursor,
        sep: Option<&TokenTree>,
    ) -> Result<DynamicStateSet> {
        self.parse_zero_or_more_repetitions_inner(states, stream, sep, true)
    }

    fn parse_zero_or_more_repetitions_inner(
        &self,
        states: DynamicStateSet,
        stream: Cursor,
        sep: Option<&TokenTree>,
        mut first: bool,
    ) -> Result<DynamicStateSet> {
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
        stream: Cursor,
        sep: Option<&TokenTree>,
    ) -> Result<DynamicStateSet> {
        let states = self.parse_single_repetition(states, None, stream)?;

        self.parse_zero_or_more_repetitions_inner(states, stream, sep, false)
    }

    fn parse_single_repetition(
        &self,
        states: DynamicStateSet,
        sep: Option<&TokenTree>,
        stream: Cursor,
    ) -> Result<DynamicStateSet> {
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
                let matcher = token_tree! { $( $matcher )* };
                let matcher = crate::matcher::TokenTree::from_generic(matcher).expect("Failed to generate `matcher::TokenTree`");
                let bindings = crate::matcher::Matcher::from_generic(&matcher).expect("Failed to generate `matcher::Bindings`").bindings;

                let subst = token_tree! { $( $substitution )* };
                let subst = crate::substitution::TokenTree::from_generic(subst).expect("Failed to generate `substitution::TokenTree`");

                let state = stringify!($kind).parse::<crate::FragmentKind>().expect("Failed to generate `FragmentKind`");
                let state = state.to_dynamic_state();

                ExpCtx::check_rule(bindings, &subst, state).expect("Checking failed");
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
