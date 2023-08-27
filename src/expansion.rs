use std::{collections::HashMap, marker::PhantomData};

use crate::{
    error::Error,
    expansion,
    grammar::{DynamicState, State},
    matcher::Matcher,
    states::DynamicStateSet,
    substitution::{TokenTree, TokenTreeKind},
    FragmentKind, RepetitionQuantifier, RepetitionQuantifierKind, Terminal,
};

type Cursor<'ast, Span> = &'ast [TokenTree<Span>];

pub(crate) fn check_arm<Span>(
    init_state: State,
    bindings: Matcher,
    substitution: &[expansion::TokenTree<Span>],
) -> Result<(), Error<Span>>
where
    Span: Copy,
{
    let bindings = bindings.bindings;
    ExpCtx::check_rule(bindings, substitution, init_state.into_dynamic_state())
}

struct ExpCtx<Span> {
    // todo: we also want to see which depth a macro repeats at.
    bindings: HashMap<String, FragmentKind>,
    span: PhantomData<Span>,
}

impl<Span> ExpCtx<Span>
where
    Span: Copy,
{
    fn check_rule(
        bindings: HashMap<String, FragmentKind>,
        subst: &[TokenTree<Span>],
        initial_state: DynamicState,
    ) -> Result<(), Error<Span>> {
        let ctxt = ExpCtx::new(bindings);
        ctxt.parse_stream(DynamicStateSet::singleton(initial_state), subst)
            .map(drop)
    }

    fn new(bindings: HashMap<String, FragmentKind>) -> ExpCtx<Span> {
        ExpCtx {
            bindings,
            span: PhantomData,
        }
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
        // TODO: we will have to deal with a huge number of transitions. We *have* to
        // find a way to generate this function, perhaps with a macro
        // :upside_down:.
        Ok(match (state.state, &tree.kind) {
            (_, TokenTreeKind::Repetition { .. }) => {
                unreachable!("Repetitions should be handled by `parse_single_tree`")
            }

            // fn
            (State::ItemStart, TokenTreeKind::Terminal(Terminal::Ident(fn_))) if fn_ == "fn" => {
                DynamicStateSet::singleton(state.with_state(State::AfterFnKw))
            }

            (State::ItemStart, _) => return Err(Error::InvalidProducedAst),

            // fn foo
            (State::AfterFnKw, TokenTreeKind::Terminal(Terminal::Ident(_))) => {
                DynamicStateSet::singleton(state.with_state(State::AfterFnName))
            }

            (State::AfterFnKw, TokenTreeKind::Fragment(name)) => {
                let Some(kind) = self.bindings.get(name) else {
                    let name = name.clone();
                    return Err(Error::UnboundMetaVariable {
                        name,
                        where_: tree.span,
                    });
                };

                if *kind != FragmentKind::Ident {
                    return Err(Error::InvalidMetaVariableContext { name: name.clone() });
                }

                DynamicStateSet::singleton(state.with_state(State::AfterFnName))
            }

            (State::AfterFnKw, _) => return Err(Error::InvalidProducedAst),

            // fn foo()
            (State::AfterFnName, TokenTreeKind::Parenthesed(params)) => {
                if self
                    .parse_stream(
                        DynamicStateSet::singleton(state.with_state(State::FnParamStart)),
                        &params,
                    )?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(Error::InvalidProducedAst);
                };

                DynamicStateSet::singleton(state.with_state(State::AfterFnParam))
            }

            // fn foo(
            (State::FnParamStart, _) => return Err(Error::InvalidProducedAst),

            (State::AfterFnName, _) => return Err(Error::InvalidProducedAst),

            // fn foo() {}
            (State::AfterFnParam, TokenTreeKind::CurlyBraced(inner)) => {
                if self
                    .parse_stream(
                        DynamicStateSet::singleton(state.with_state(State::ExprStart)),
                        &inner,
                    )?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(Error::InvalidProducedAst);
                }

                DynamicStateSet::singleton(state.with_state(State::ItemStart))
            }

            (State::AfterFnParam, _) => return Err(Error::InvalidProducedAst),

            (State::ExprStart, TokenTreeKind::Terminal(Terminal::Ident(_))) => {
                DynamicStateSet::singleton(state.with_state(State::AfterBinop))
            }

            (State::ExprStart, TokenTreeKind::Parenthesed(inner)) => {
                if self
                    .parse_stream(
                        DynamicStateSet::singleton(state.with_state(State::ExprStart)),
                        &inner,
                    )?
                    .into_iter()
                    .any(|state| !state.is_accepting())
                {
                    return Err(Error::InvalidProducedAst);
                };

                DynamicStateSet::singleton(state.with_state(State::AfterBinop))
            }

            (State::ExprStart, TokenTreeKind::Fragment(name)) => {
                let Some(kind) = self.bindings.get(name) else {
                    let name = name.clone();
                    return Err(Error::UnboundMetaVariable {
                        name,
                        where_: tree.span,
                    });
                };

                match kind {
                    FragmentKind::Ident => {
                        DynamicStateSet::singleton(state.with_state(State::AfterBinop))
                    }
                    FragmentKind::Expr => {
                        DynamicStateSet::singleton(state.with_state(State::AfterBinop))
                    }
                    FragmentKind::Item => {
                        return Err(Error::InvalidMetaVariableContext { name: name.clone() })
                    }
                }
            }

            (State::ExprStart, _) => return Err(Error::InvalidProducedAst),

            (State::AfterBinop, TokenTreeKind::Terminal(Terminal::Plus | Terminal::Times)) => {
                DynamicStateSet::singleton(state.with_state(State::ExprStart))
            }

            (State::AfterBinop, _) => return Err(Error::InvalidProducedAst),
        })
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
