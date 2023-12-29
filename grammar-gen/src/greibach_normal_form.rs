use std::collections::HashSet;

use crate::grammar_maker::{Expr, Rule};

pub(crate) fn greibachify(rules: Vec<Rule>) -> Vec<Rule> {
    let rules = cancel_epsilon_rules(rules);
    let rules = remove_unit_rules(rules);

    todo!();
}

fn cancel_epsilon_rules(rules: Vec<Rule>) -> Vec<Rule> {
    let nullables = dbg!(nullable_rules(&rules));

    let mut out = Vec::new();

    for epsilon_rule in nullables {
        for rule in rules.iter() {
            out.extend(permutations(rule.clone(), epsilon_rule));
        }
    }

    out
}

fn nullable_rules(rules: &[Rule]) -> HashSet<&str> {
    rules
        .iter()
        .filter(|rule| is_epsilon(rule))
        .map(|rule| rule.name.as_str())
        .collect::<HashSet<_>>()
}

fn permutations(rule: Rule, epsilons_rules: &str) -> Vec<Rule> {
    if rule.seq.is_empty() {
        return Vec::new();
    }

    let name = rule.name;
    let mut out = RulesPermutationsGenerator::new();

    for expr in rule.seq {
        if matches!(&expr, Expr::Rule(name) if name == epsilons_rules) {
             out = out.append_epsilon(expr);
        } else {
            out = out.append(expr);
        }
    }

    out.with_rule_name(name)
}

struct RulesPermutationsGenerator {
    rules: Vec<Vec<Expr>>,
}

impl RulesPermutationsGenerator {
    fn new() -> RulesPermutationsGenerator {
        RulesPermutationsGenerator {
            rules: Vec::new(),
        }
    }

    fn with_rule_name(self, name: String) -> Vec<Rule> {
        self.rules
            .into_iter()
            .map(|seq| Rule { name: name.clone(), seq })
            .collect()
    }

    fn append(mut self, expr: Expr) -> RulesPermutationsGenerator {
        self.rules.iter_mut().for_each(|rule| {
            rule.push(expr.clone());
        });

        self
    }

    fn append_epsilon(mut self, expr: Expr) -> RulesPermutationsGenerator {
        let tail = self.rules.iter().cloned().map(|mut rule| {
            rule.push(expr.clone());
            rule
        }).collect::<Vec<_>>();

        self.rules.extend(tail);

        self
    }
}

fn is_epsilon(rule: &Rule) -> bool {
    rule.seq.is_empty()
}

fn remove_unit_rules(rules: Vec<Rule>) -> Vec<Rule> {
    let mut out = Vec::new();

    out
}