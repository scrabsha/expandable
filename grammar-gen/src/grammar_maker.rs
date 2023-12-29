use std::{fmt, iter, sync::Mutex};
use std::fmt::{Display, Formatter};

use crate::{
    common::{Fragment, Token},
    spec_parser,
    spec_parser::{Quantifier, Specifications},
};

// TODO: we may be able to speed this step up by passing specifications by
// reference.

pub(crate) fn build_from_specs(specs: Specifications) -> Grammar {
    let rules = specs.rules.into_iter().flat_map(linearize_rule).collect();

    Grammar { rules }
}

fn linearize_rule(rule: spec_parser::Rule) -> Vec<Rule> {
    let name = rule.name;
    let empty_rule = Rule::epsilon(name);

    let LinearizedRule {
        linearizations,
        internal_rules,
    } = linearize_expr(empty_rule, rule.expr);

    linearizations
        .into_iter()
        .chain(internal_rules.into_iter())
        .collect()
}

fn linearize_expr(current_rule: Rule, expr: spec_parser::Expr) -> LinearizedRule {
    match expr {
        spec_parser::Expr::Seq(seq) => {
            let mut outcomes = LinearizedRule::linear_singleton(current_rule);

            for expr in seq {
                let outcomes_ = outcomes
                    .linearizations
                    .into_iter()
                    .map(|rule| linearize_expr(rule, expr.clone()));
                outcomes =
                    LinearizedRule::merge(outcomes_).extend_internals(outcomes.internal_rules);
            }

            outcomes
        }

        spec_parser::Expr::Alt(alts) => {
            let outcomes = alts
                .into_iter()
                .map(|alt| linearize_expr(current_rule.clone(), alt));

            LinearizedRule::merge(outcomes)
        }

        spec_parser::Expr::Rep(expr, quantifier) => {
            match quantifier {
                Quantifier::ZeroOrOne => {
                    // We turn:
                    //   A -> B? C
                    // Into:
                    //   A -> B C
                    //   A -> C

                    let zero = current_rule.clone().to_linealized_rule();
                    let one = linearize_expr(current_rule, *expr);

                    LinearizedRule::merge([zero, one])
                }

                Quantifier::ZeroOrMore => {
                    // We turn:
                    //   A -> B* C
                    // Into:
                    //   A -> A_0 C
                    //   A -> C
                    //   A_0 -> B
                    //   A_0 -> B A_0
                    //
                    // We do this by cheating and turning the initial input
                    // into:
                    //   A -> B+ C
                    //   A -> C
                    //
                    // And relying on the OneOrMore case to do the rest.
                    let outcomes = linearize_expr(
                        current_rule.clone(),
                        spec_parser::Expr::Rep(expr, Quantifier::OneOrMore),
                    );
                    let zero = current_rule.to_linealized_rule();

                    LinearizedRule::merge([zero, outcomes])
                    // let aux_rule_name = gensym(&current_rule.name);
                    //
                    // let main_0 = Rule {
                    //     name: current_rule.name.clone(),
                    //     seq: current_rule.seq.clone(),
                    // };
                    //
                    // let mut main_1_seq = current_rule.seq;
                    // main_1_seq.push(Expr::Rule(aux_rule_name.clone()));
                    // let main_1 = Rule {
                    //     name: current_rule.name,
                    //     seq: main_1_seq,
                    // };
                    //
                    //
                    //
                    // let aux_rule = Rule {
                    //     name: aux_rule_name.clone(),
                    //     seq: vec![],
                    // };
                    // let aux_spec = spec_parser::Expr::Alt(vec![
                    //     spec_parser::Expr::Seq(vec![*expr,
                    // spec_parser::Expr::Rule(aux_rule_name)]),
                    //     spec_parser::Expr::Seq(Vec::new()),
                    // ]);
                    //
                    // let aux_rules = linearize_expr(aux_rule, aux_spec);
                    //
                    // iter::once(main).chain(aux_rules).collect()
                }

                Quantifier::OneOrMore => {
                    // We turn:
                    //   A -> B+ C
                    // Into:
                    //   A -> A_0 C
                    //   A_0 -> B
                    //   A_0 -> B A_0

                    let aux_rule_name = gensym(&current_rule.name);
                    let mut seq = current_rule.seq.clone();
                    seq.push(Expr::Rule(aux_rule_name.clone()));
                    let main = Rule {
                        name: current_rule.name,
                        seq,
                    }
                    .to_linealized_rule();

                    let aux_tmp = Rule {
                        name: aux_rule_name.clone(),
                        seq: vec![],
                    };
                    // Rules computed from A_0 -> B
                    let auxs_0 = linearize_expr(aux_tmp, *expr);

                    // Rules computed from A_0 -> B A_0
                    let auxs_1 = auxs_0
                        .linearizations
                        .iter()
                        .map(|rule| {
                            let mut seq = rule.seq.clone();
                            seq.push(Expr::Rule(aux_rule_name.clone()));

                            let name = rule.name.clone();

                            Rule { name, seq }
                        })
                        .collect::<Vec<_>>();

                    let auxs_1 = LinearizedRule {
                        linearizations: auxs_1,
                        internal_rules: Vec::new(),
                    };

                    LinearizedRule::merge([main, auxs_0, auxs_1])
                }
            }
        }

        spec_parser::Expr::Token(token) => {
            let mut current_rule_seq = current_rule.seq;
            current_rule_seq.push(Expr::Token(token));

            Rule {
                name: current_rule.name,
                seq: current_rule_seq,
            }
            .to_linealized_rule()
        }

        spec_parser::Expr::Rule(name) => {
            let mut current_rule_seq = current_rule.seq;
            current_rule_seq.push(Expr::Rule(name.clone()));

            Rule {
                name: current_rule.name,
                seq: current_rule_seq,
            }
            .to_linealized_rule()
        }

        spec_parser::Expr::Fragment(fragment) => {
            let mut current_rule_seq = current_rule.seq;
            current_rule_seq.push(Expr::Fragment(fragment.clone()));

            Rule {
                name: current_rule.name,
                seq: current_rule_seq,
            }
            .to_linealized_rule()
        }
    }
}

fn gensym(base: &str) -> String {
    static COUNTER: Mutex<usize> = Mutex::new(0);

    let mut counter = COUNTER.lock().unwrap();
    let sym = format!("{}_{}", base, *counter);
    *counter += 1;

    sym
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Grammar {
    pub(crate) rules: Vec<Rule>,
}

impl Display for Grammar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for rule in &self.rules {
            writeln!(f, "{}", rule)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LinearizedRule {
    linearizations: Vec<Rule>,
    internal_rules: Vec<Rule>,
}

impl LinearizedRule {
    fn linear_singleton(rule: Rule) -> LinearizedRule {
        LinearizedRule {
            linearizations: vec![rule],
            internal_rules: Vec::new(),
        }
    }

    fn append(mut self, expr: &Expr) -> LinearizedRule {
        self.linearizations.iter_mut().for_each(|rule| {
            rule.seq.push(expr.clone());
        });

        self
    }

    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Rule>,
    {
        self.linearizations.extend(iter);
    }

    fn merge<I>(rules: I) -> LinearizedRule
    where
        I: IntoIterator<Item = LinearizedRule>,
    {
        let mut tmp = LinearizedRule {
            linearizations: Vec::new(),
            internal_rules: Vec::new(),
        };

        for rule in rules {
            tmp.linearizations.extend(rule.linearizations);
            tmp.internal_rules.extend(rule.internal_rules);
        }

        tmp
    }

    fn extend_internals(mut self, internals: Vec<Rule>) -> LinearizedRule {
        let mut internal_rules = self.internal_rules;
        internal_rules.extend(internals);

        LinearizedRule {
            linearizations: self.linearizations,
            internal_rules,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Rule {
    pub(crate) name: String,
    pub(crate) seq: Vec<Expr>,
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.name)?;

        for (i, expr) in self.seq.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }

            match expr {
                Expr::Token(token) => write!(f, "{}", token)?,
                Expr::Rule(name) => write!(f, "{}", name)?,
                Expr::Fragment(fragment) => write!(f, "{}", fragment)?,
            }
        }

        Ok(())
    }
}

impl Rule {
    fn to_linealized_rule(self) -> LinearizedRule {
        LinearizedRule {
            linearizations: vec![self],
            internal_rules: Vec::new(),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &Expr> {
        self.seq.iter()
    }
}

impl Rule {
    fn epsilon(name: String) -> Rule {
        Rule {
            name,
            seq: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Token(Token),
    Rule(String),
    Fragment(Fragment),
}

impl Expr {
    pub(crate) fn is_terminal(&self) -> bool {
        match self {
            Expr::Token(_) => true,
            Expr::Rule(_) => false,
            Expr::Fragment(_) => true,
        }
    }
    
    pub(crate) fn is_nonterminal(&self) -> bool {
        !self.is_terminal()
    }
}