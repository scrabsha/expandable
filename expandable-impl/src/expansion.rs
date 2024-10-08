// Architectural invariant: this module contains the parsing logic for macro
// transcribers.

use std::{
    cell::RefCell,
    cmp::Ord,
    collections::{BTreeSet, HashMap},
    iter,
};

use crate::{
    RepetitionQuantifier, RepetitionQuantifierKind, TokenDescription,
    error::Error,
    grammar::{DynamicState, ParsingError, Transition},
    matcher::{BindingData, Matcher},
    substitution::{TokenTree, TokenTreeKind},
};

type Cursor<'ast, Span> = &'ast [TokenTree<Span>];
type Set<T> = BTreeSet<T>;

pub(crate) fn check_arm<Span>(
    init_state: DynamicState<Span>,
    bindings: Matcher<Span>,
    substitution: &[TokenTree<Span>],
    closing_brace_span: Span,
) -> Result<(), Error<Span>>
where
    Span: Copy + 'static,
{
    let bindings = bindings.bindings;
    ExpCtx::check_rule(bindings, substitution, init_state, closing_brace_span)
}

struct ExpCtx<Span> {
    // todo: we also want to see which depth a macro repeats at.
    bindings: HashMap<String, BindingData<Span>>,
    id: RefCell<usize>,
}

impl<Span> ExpCtx<Span>
where
    Span: Copy + 'static,
{
    // This value allows us to ensure that the check does not hang even if
    // there's a bug in the state machine logic. It is used in any fixed-point
    // iteration in the codebase.
    //
    // Parsing code is so fragile that a low value should be enough for us to
    // either find the fixed-point or detect a syntax error.
    const FIXED_POINT_ITERATIONS_FUEL: usize = 16;

    fn check_rule(
        bindings: HashMap<String, BindingData<Span>>,
        subst: &[TokenTree<Span>],
        initial_state: DynamicState<Span>,
        closing_brace_span: Span,
    ) -> Result<(), Error<Span>> {
        let ctx = ExpCtx::new(bindings);
        let states = ctx.parse_stream(singleton((initial_state, ctx.id())), subst)?;

        states
            .into_iter()
            .try_for_each(|(mut state, _, _)| state.is_accepting(closing_brace_span))
            .map_err(|e| CounterExamplePrefix::from(e).into())
    }

    fn new(bindings: HashMap<String, BindingData<Span>>) -> ExpCtx<Span> {
        ExpCtx {
            bindings,
            id: RefCell::default(),
        }
    }

    fn id(&self) -> usize {
        let mut id = self.id.borrow_mut();
        let id_ = *id;

        *id += 1;

        id_
    }

    fn parse_single_tree<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        tree: &TokenTree<Span>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
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
                .map(|(state, id)| self.parse_single_tree_inner(state, tree, id))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect()),
        }
    }

    fn parse_single_tree_inner<Id>(
        &self,
        state: DynamicState<Span>,
        tree: &TokenTree<Span>,
        id: Id,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        match &tree.kind {
            TokenTreeKind::Terminal(_, descr) => state
                .accept(*descr, tree.span)
                .map_err(CounterExamplePrefix::from)
                .map(|(s, t)| (s, t, id))
                .map(singleton),

            TokenTreeKind::Parenthesed(inner) => self.check_delimited_stream(
                TokenDescription::LParen,
                inner,
                TokenDescription::RParen,
                tree.span,
                state,
                id,
            ),

            TokenTreeKind::CurlyBraced(inner) => self.check_delimited_stream(
                TokenDescription::LBrace,
                inner,
                TokenDescription::RBrace,
                tree.span,
                state,
                id,
            ),

            TokenTreeKind::Bracketed(inner) => self.check_delimited_stream(
                TokenDescription::LBracket,
                inner,
                TokenDescription::RBracket,
                tree.span,
                state,
                id,
            ),

            TokenTreeKind::Fragment(f) => {
                // This is safe because we ensured at the previous step that
                // a fragment with that name indeed exists.
                let kind = self.bindings.get(f).expect("Fragment not found").kind;
                state
                    .accept_fragment(kind, tree.span)
                    .map(|(s, t)| (s, t, id))
                    .map(singleton)
                    .map_err(CounterExamplePrefix::from)
            }

            TokenTreeKind::Repetition { .. } => {
                unreachable!("Repetitions should be handled by `ExpCtx::parse_single_tree`")
            }
        }
    }

    fn check_delimited_stream<Id>(
        &self,
        open: TokenDescription,
        inner: &[TokenTree<Span>],
        close: TokenDescription,
        span: Span,
        initial_state: DynamicState<Span>,
        id: Id,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        // Parse open delimiter
        let (s_after_open_delim, t_after_open_delim) = initial_state
            .clone()
            .accept(open, span)
            .map_err(|parsing_error| {
                let mut cex = CounterExamplePrefix::from(parsing_error);
                cex.push_stream(&self.bindings, inner);
                cex.push(close, span);

                cex
            })?;

        let states = self
            .parse_stream(singleton((s_after_open_delim, id)), inner)
            .map_err(|mut cex| {
                cex.push(close, span);
                cex
            })?;

        let states = states
            .into_iter()
            .map(|(s, t, id)| (s, t_after_open_delim.clone() + t, id));

        // Parse close delimiter
        let states = states
            .into_iter()
            .map(|(state, trans, id)| {
                state
                    .accept(close, span)
                    .map(|(state, new_trans)| {
                        let trans = trans + new_trans;
                        (state, trans, id)
                    })
                    .map_err(CounterExamplePrefix::from)
            })
            .collect::<Result<_, _>>()?;

        Ok(states)
    }

    fn parse_stream<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        stream: Cursor<Span>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        let mut states: Set<(DynamicState<Span>, (Transition, Id))> = states
            .into_iter()
            .map(|(s, id)| {
                let trans = s.initial_transition();
                (s, (trans, id))
            })
            .collect();

        for (idx, tree) in stream.iter().enumerate() {
            states = self
                .apply(states, |this, states| this.parse_single_tree(states, tree))
                .map_err(|mut cex| {
                    if idx < stream.len() {
                        // SUBTLE: the pushdown automaton has a lookhead of 2
                        // tokens. This means the error is always detected
                        // "two tokens after" it actually occurs.
                        //
                        // However this is not an issue here. We found an
                        // invalid expansion, and it will stay invalid no
                        // matter what comes after it. Sweet!
                        cex.push_stream(&self.bindings, &stream[(idx + 1)..]);
                    }

                    cex
                })?
                .into_iter()
                .map(|(state, trans_, (trans, id))| (state, (trans + trans_, id)))
                .collect();
        }

        let states = states.into_iter().map(|(s, (t, id))| (s, t, id)).collect();

        Ok(states)
    }

    fn parse_repetition<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
        quantifier: RepetitionQuantifier<Span>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
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

    fn parse_zero_or_one_repetitions<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        stream: Cursor<Span>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        let mut candidates = states
            .into_iter()
            .map(|(s, id)| {
                let trans = s.initial_transition();
                (s, trans, id)
            })
            .collect::<Set<_>>();

        let candidates_ = candidates
            .iter()
            .cloned()
            .map(|(s, t, id)| (s, (t, id)))
            .collect::<Set<_>>();

        candidates.extend(
            self.parse_single_repetition(candidates_, None, stream)?
                .into_iter()
                .map(|(s, t_, (t, id))| (s, t + t_, id)),
        );

        Ok(candidates)
    }

    fn parse_zero_or_more_repetitions<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        self.parse_zero_or_more_repetitions_inner(states, stream, sep, true)
    }

    fn parse_zero_or_more_repetitions_inner<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
        mut first: bool,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        // Values accumulated during the previous iterations.
        let mut outcomes = states
            .iter()
            .cloned()
            .map(|(s, id)| {
                let trans = s.initial_transition();
                (s, trans, id)
            })
            .collect::<Set<_>>();

        let mut reached_transitions = Set::<Transition>::new();
        // Values discovered during the current iteration.
        let mut to_test: Set<(DynamicState<_>, (Transition, Id))> = states
            .into_iter()
            .map(|(s, id)| {
                let trans = s.initial_transition();
                (s, (trans, id))
            })
            .collect();

        let mut fuel = Self::FIXED_POINT_ITERATIONS_FUEL;

        while !to_test.is_empty() {
            assert!(fuel > 0, "No remaining fuel. This is a bug.");
            fuel -= 1;

            let sep = if first { None } else { sep };

            let (to_test_, reached_transitions_) = self
                .apply(to_test.clone(), |this, states| {
                    this.parse_single_repetition(states, sep, stream)
                })?
                .into_iter()
                .filter_map(|(s, t_, (t, id))| {
                    if !reached_transitions.contains(&t_) {
                        Some(((s, (t + t_.clone(), id)), t_))
                    } else {
                        None
                    }
                })
                .unzip::<_, _, Set<_>, Vec<_>>();

            to_test.clone_from(&to_test_);
            outcomes.extend(to_test_.into_iter().map(|(s, (t, id))| (s, t, id)));

            reached_transitions.extend(reached_transitions_);

            first = false;
        }

        Ok(outcomes)
    }

    fn parse_one_or_more_repetitions<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        stream: Cursor<Span>,
        sep: Option<&TokenTree<Span>>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        let states = self
            .parse_single_repetition(states, None, stream)?
            .into_iter()
            .map(|(s, t, id)| (s, (t, id)))
            .collect();

        Ok(self
            .parse_zero_or_more_repetitions_inner(states, stream, sep, false)?
            .into_iter()
            .map(|(s, t_, (t, id))| (s, t + t_, id))
            .collect())
    }

    fn parse_single_repetition<Id>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        sep: Option<&TokenTree<Span>>,
        stream: Cursor<Span>,
    ) -> Result<Set<(DynamicState<Span>, Transition, Id)>, CounterExamplePrefix<Span>>
    where
        Id: Clone + Ord,
    {
        let states = match sep {
            Some(sep) => {
                let mut buf = Set::new();

                for (state, id) in states {
                    let s = self.parse_single_tree_inner(state, sep, id)?;

                    for (s, t, id) in s {
                        buf.insert((s, t, id.clone()));
                    }
                }

                buf
            }

            None => states
                .into_iter()
                .map(|(s, id)| {
                    let trans = s.initial_transition();
                    (s, trans, id)
                })
                .collect(),
        };

        let states = states.into_iter().map(|(s, t, id)| (s, (t, id))).collect();

        Ok(self
            .parse_stream(states, stream)?
            .into_iter()
            .map(|(s, t_, (t, id))| (s, t + t_, id))
            .collect())
    }

    fn apply<F, Id, Id_>(
        &self,
        states: Set<(DynamicState<Span>, Id)>,
        mut f: F,
    ) -> Result<Set<(DynamicState<Span>, Id_, Id)>, CounterExamplePrefix<Span>>
    where
        F: FnMut(
            &Self,
            Set<(DynamicState<Span>, usize)>,
        )
            -> Result<Set<(DynamicState<Span>, Id_, usize)>, CounterExamplePrefix<Span>>,
        Id: Clone + Ord,
        Id_: Clone + Ord,
    {
        // Let's use usize as new id :)
        let (states, joint) = states
            .into_iter()
            .enumerate()
            .map(|(new_id, (s, id))| ((s, new_id), id))
            .unzip::<_, _, Set<_>, Vec<_>>();

        let output = f(self, states)?;

        Ok(output
            .into_iter()
            .map(|(s, id_, id)| (s, id_, joint.get(id).expect("Unknown id 👀").clone()))
            .collect())
    }
}

impl<Span> From<CounterExamplePrefix<Span>> for Error<Span> {
    fn from(cex: CounterExamplePrefix<Span>) -> Error<Span> {
        Error::InvalidProducedAst {
            span: cex.err_span,
            expected: cex.expected,
            counter_example: cex.prefix,
        }
    }
}

fn singleton<T>(t: T) -> Set<T>
where
    T: Ord,
{
    iter::once(t).collect()
}

#[derive(Clone, PartialEq)]
struct CounterExamplePrefix<Span> {
    err_span: Span,
    expected: Vec<TokenDescription>,
    prefix: Vec<(TokenDescription, Span)>,
}

impl<Span> From<ParsingError<Span>> for CounterExamplePrefix<Span> {
    fn from(
        ParsingError {
            err_span,
            expected,
            cex: prefix,
        }: ParsingError<Span>,
    ) -> CounterExamplePrefix<Span> {
        CounterExamplePrefix {
            err_span,
            expected,
            prefix,
        }
    }
}

impl<Span> CounterExamplePrefix<Span>
where
    Span: Copy,
{
    fn push(&mut self, descr: TokenDescription, span: Span) {
        self.prefix.push((descr, span));
    }

    fn push_stream(
        &mut self,
        tbl: &HashMap<String, BindingData<Span>>,
        stream: &[TokenTree<Span>],
    ) {
        for tree in stream {
            let span = tree.span;
            match &tree.kind {
                TokenTreeKind::Terminal(_, descr) => self.push(*descr, tree.span),
                TokenTreeKind::Parenthesed(inner) => self.push_delimited_stream(
                    tbl,
                    TokenDescription::LParen,
                    span,
                    inner,
                    TokenDescription::RParen,
                    span,
                ),

                TokenTreeKind::CurlyBraced(inner) => self.push_delimited_stream(
                    tbl,
                    TokenDescription::LBrace,
                    span,
                    inner,
                    TokenDescription::RBrace,
                    span,
                ),

                TokenTreeKind::Bracketed(inner) => self.push_delimited_stream(
                    tbl,
                    TokenDescription::LBracket,
                    span,
                    inner,
                    TokenDescription::RBracket,
                    span,
                ),

                TokenTreeKind::Fragment(name) => {
                    self.push(TokenDescription::Fragment(tbl[name].kind), span);
                }

                TokenTreeKind::Repetition {
                    quantifier:
                        RepetitionQuantifier {
                            kind:
                                RepetitionQuantifierKind::ZeroOrOne
                                | RepetitionQuantifierKind::ZeroOrMore,
                            ..
                        },
                    ..
                } => {}

                TokenTreeKind::Repetition {
                    inner,
                    quantifier:
                        RepetitionQuantifier {
                            kind: RepetitionQuantifierKind::OneOrMore,
                            ..
                        },
                    ..
                } => self.push_stream(tbl, inner),
            }
        }
    }

    fn push_delimited_stream(
        &mut self,
        tbl: &HashMap<String, BindingData<Span>>,
        open_descr: TokenDescription,
        open_span: Span,
        inner: &[TokenTree<Span>],
        close_descr: TokenDescription,
        close_span: Span,
    ) {
        self.push(open_descr, open_span);
        self.push_stream(tbl, inner);
        self.push(close_descr, close_span);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_valid_arm {
        ( $test_name:ident { #[$kind:ident]( $( $matcher:tt )* ) => { $( $substitution:tt )* } $(;)? }) => {
            #[test]
            fn $test_name() {
                let matcher: Vec<crate::TokenTree<_>> = quote! { $( $matcher )* };
                let matcher = crate::matcher::TokenTree::from_generic(matcher).expect("Failed to generate `matcher::TokenTree`");
                let bindings = crate::matcher::Matcher::from_generic(&matcher).expect("Failed to generate `matcher::Bindings`");

                let subst = quote! { $( $substitution )* };
                let subst = crate::substitution::TokenTree::from_generic(subst).expect("Failed to generate `substitution::TokenTree`");

                let state = stringify!($kind).parse::<crate::InvocationContext>().expect("Failed to generate `FragmentKind`");
                let state = state.to_state();

                check_arm(state, bindings, &subst, crate::span::DebugSpan::EOF).expect("Checking failed");
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
            () => { a * b + c }
        }
    }

    assert_valid_arm! {
        with_fragment {
            #[expr]
            ( #a:ident ) => { #a }
        }
    }

    assert_valid_arm! {
        with_repetition_question {
            #[expr]
            () => { a #( * b )? }
        }
    }

    assert_valid_arm! {
        with_repetition_plus {
            #[expr]
            ( #a:ident ) => { a #( * )b+ c }
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
            (#name:ident) => { fn #name() { a } }
        }
    }

    assert_valid_arm! {
        custom_function {
            #[item]
            (
                fn #name:ident() { #body:expr }
            ) => {
                fn #name() { #body }
            }
        }
    }

    assert_valid_arm! {
        fn_with_arg {
            #[item]
            () => {
                fn test(a: u8) { 101 }
            }
        }
    }

    assert_valid_arm! {
        expr_path_full_fragment {
            #[expr]
            ( #ident:ident ) => {
                #( :: #ident )+
            }
        }
    }

    macro_rules! assert_cex {
        ( $test_name:ident { #[$kind:ident]( $( $matcher:tt )* ) => { $( $substitution:tt )* }; $expect:expr }) => {
            #[test]
            fn $test_name() {
                let matcher: Vec<crate::TokenTree<_>> = quote! { $( $matcher )* };
                let matcher = crate::matcher::TokenTree::from_generic(matcher).expect("Failed to generate `matcher::TokenTree`");
                let bindings = crate::matcher::Matcher::from_generic(&matcher).expect("Failed to generate `matcher::Bindings`");

                let subst = quote! { $( $substitution )* };
                let subst = crate::substitution::TokenTree::from_generic(subst).expect("Failed to generate `substitution::TokenTree`");

                let state = stringify!($kind).parse::<crate::InvocationContext>().expect("Failed to generate `FragmentKind`");
                let state = state.to_state();

                let e = check_arm(state, bindings, &subst, crate::span::DebugSpan::EOF).unwrap_err();

                let left = match e {
                    Error::InvalidProducedAst { counter_example, .. } => counter_example,
                    _ => panic!("Unexpected error variant: {:?}", e),
                };

                let left = left.into_iter().map(|(tok, _)| tok).collect::<Vec<_>>();

                let right = $expect;

                right.assert_debug_eq(&left);
            }
        }
    }

    assert_cex! {
        cex_simple_plusplus {
            #[expr]
            () => {
                4 ++ 5
            };
            expect_test::expect![[r#"
                [
                    Literal,
                    Plus,
                    Plus,
                    Literal,
                ]
            "#]]
        }
    }

    assert_cex! {
        cex_with_repetition {
            #[expr]
            () => {
                #( 42 + )* + a
            };
            expect_test::expect![[r#"
                [
                    Plus,
                    Ident,
                ]
            "#]]
        }
    }

    assert_cex! {
        // Performs an expansion check of the following macro:
        //
        // ```rust
        // macro_rules! cex_with_fragment {
        //     ($a:ident) => {
        //         $( $a ++ )*
        //     }
        // }
        // ```
        //
        // This macro expands may expand to invalid code, because Rust has no
        // `++` operator. A possible expansion of this macro that exhibits this
        // error is the following:
        //
        // ```
        // a ++ b ++
        // ```
        cex_with_fragment {
            #[expr]
            (#a:ident) => {
                #( #a ++ )*
            };
            expect_test::expect![[r#"
                [
                    Fragment(
                        Ident,
                    ),
                    Plus,
                    Plus,
                    Fragment(
                        Ident,
                    ),
                    Plus,
                    Plus,
                ]
            "#]]
        }
    }
}
