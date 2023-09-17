use crate::list::LameLinkedList;
use crate::matcher::{BindingData, Matcher};
use crate::substitution::{TokenTree, TokenTreeKind};
use crate::{Error, RepetitionQuantifierKind};

pub(crate) fn check<Span>(
    matcher: &Matcher<Span>,
    substitution: &[TokenTree<Span>],
) -> Result<(), Error<Span>>
where
    Span: Copy,
{
    let usages = substitution
        .iter()
        .flat_map(|e| collect_usages(e, &LameLinkedList::Nil));

    for (name, usage_span, stack) in usages {
        let binding_data = matcher.bindings.get(name);

        match binding_data {
            Some(BindingData {
                repetition_stack,
                span,
                ..
            }) => {
                if &stack != repetition_stack {
                    return Err(Error::InvalidRepetitionNesting {
                        metavariable_name: name.to_string(),
                        decl_span: *span,
                        usage_span,
                        expected_nesting: repetition_stack.clone(),
                        got_nesting: stack,
                    });
                }
            }

            None => {
                return Err(Error::UnboundMetavariable {
                    name: name.to_string(),
                    where_: usage_span,
                })
            }
        }
    }

    Ok(())
}

fn collect_usages<'ast, Span>(
    elem: &'ast TokenTree<Span>,
    stack: &LameLinkedList<RepetitionQuantifierKind>,
) -> Vec<(&'ast str, Span, Vec<RepetitionQuantifierKind>)>
where
    Span: Copy,
{
    match &elem.kind {
        TokenTreeKind::Terminal(_) => Vec::new(),
        TokenTreeKind::Parenthesed(inner) | TokenTreeKind::CurlyBraced(inner) => inner
            .iter()
            .flat_map(|elem| collect_usages(elem, stack))
            .collect(),

        TokenTreeKind::Fragment(name) => vec![(name.as_str(), elem.span, stack.to_vec())],

        TokenTreeKind::Repetition {
            inner, quantifier, ..
        } => {
            let stack = LameLinkedList::Cons(quantifier.kind, stack);
            inner
                .iter()
                .flat_map(|elem| collect_usages(elem, &stack))
                .collect()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! repetition_match_test {
        (
            $( #[ $meta:meta ] )*
            fn $name:ident() {
                {
                    $matcher:tt => $substitution:tt
                }
            }
        ) => {
            $( #[ $meta ] )*
            #[test]
            fn $name() {
                let matcher = quote!( $matcher );
                let substitution = quote!( $substitution );

                let matcher = $crate::matcher::TokenTree::from_generic(matcher).expect("Failed to create matcher TokenStream");
                let matcher = $crate::matcher::Matcher::from_generic(&matcher).expect("Failed to create a matcher Matcher object");

                let substitution = $crate::substitution::TokenTree::from_generic(substitution).expect("Failed to create substitution TokenStream");

                check(&matcher, &substitution).unwrap()
            }
        };
    }

    repetition_match_test! {
        fn no_repetition_test() {
            {
                (@a:ident) => {@a}
            }
        }
    }

    repetition_match_test! {
        fn with_multiple_repetitions() {
            {
                ( @( @( @( @a:ident )? )+ )* ) => ( @( @( @( @a )? )+ )* )
            }
        }
    }

    repetition_match_test! {
        #[should_panic(expected = "called `Result::unwrap()` on an `Err` value: \
            InvalidRepetitionNesting { \
                metavariable_name: \"a\", \
                decl_span: (), \
                usage_span: (), \
                expected_nesting: [ZeroOrMore], \
                got_nesting: [OneOrMore] \
            }\
        ")]
        fn nonmatching_stack() {
            {
                ( @( @a:ident )* ) => { @( @a )+ }
            }
        }
    }
}
