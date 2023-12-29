use ::pest::iterators::Pair;
use anyhow::{Context, Result};
use pest::{iterators::Pairs, Parser};

use crate::common::{Fragment, Token};

mod pest_ {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "grammar.pest"]
    pub struct SpecParser;
}

pub(crate) fn parse_specs(content: &str) -> Result<Specifications> {
    let document = pest_::SpecParser::parse(pest_::Rule::Document, content)
        .context("Failed to parse grammar spec")?
        .next()
        .context("Expected a document")?;

    let rules = document
        .into_inner()
        .map(|rule| match rule.as_rule() {
            pest_::Rule::Rule => {
                let mut rule = rule.into_inner();
                let name = rule.next().unwrap().as_str().to_owned();
                let expr = rule.next().unwrap();
                let expr = parse_expr(expr);
                Rule { name, expr }
            }

            rule => unreachable!("A document is supposed to contain only rules; got {rule:?}"),
        })
        .collect();

    Ok(Specifications { rules })
}

fn parse_expr(e: Pair<pest_::Rule>) -> Expr {
    match e.as_rule() {
        pest_::Rule::RuleBody => parse_expr(e.into_inner().next().unwrap()),

        pest_::Rule::OrExpression => mk_group_expr(Expr::Alt, e.into_inner()),

        pest_::Rule::AndExpression => mk_group_expr(Expr::Seq, e.into_inner()),

        pest_::Rule::Term => {
            let mut pairs = e.into_inner();
            let expr = parse_expr(pairs.next().unwrap());
            let quantifier = pairs.next().map(parse_quantifier);

            match quantifier {
                Some(quantifier) => Expr::Rep(Box::new(expr), quantifier),
                None => expr,
            }
        }

        pest_::Rule::Atom => parse_expr(e.into_inner().next().unwrap()),

        pest_::Rule::Token => parse_expr(e.into_inner().next().unwrap()),

        pest_::Rule::Punctuation => match e.as_str() {
            "`::`" => Expr::Token(Token::ColonColon),
            "`<`" => Expr::Token(Token::LessThan),
            "`>`" => Expr::Token(Token::GreaterThan),
            "`,`" => Expr::Token(Token::Comma),

            _ => unreachable!("Unexpected punctuation: {e:?}"),
        },

        pest_::Rule::RuleName => Expr::Rule(e.as_str().to_owned()),

        pest_::Rule::Repetition => {
            let mut pairs = e.into_inner();
            let expr = parse_expr(pairs.next().unwrap());
            let quantifier = parse_quantifier(pairs.next().unwrap());

            Expr::Rep(Box::new(expr), quantifier)
        }

        pest_::Rule::Fragment => match e.as_str() {
            "<ident>" => Expr::Fragment(Fragment::Ident),
            "<literal>" => Expr::Fragment(Fragment::Literal),
            "<lifetime>" => Expr::Fragment(Fragment::Lifetime),

            _ => unreachable!("Unexpected fragment: {e:?}"),
        },

        rule => unreachable!("Unexpected rule: {rule:?}"),
    }
}

fn parse_quantifier(quantifier: Pair<pest_::Rule>) -> Quantifier {
    match quantifier.as_rule() {
        pest_::Rule::Quantifier => match quantifier.as_str() {
            "?" => Quantifier::ZeroOrOne,
            "*" => Quantifier::ZeroOrMore,
            "+" => Quantifier::OneOrMore,
            _ => unreachable!("Unexpected quantifier: {quantifier:?}"),
        },

        rule => unreachable!("Unexpected rule: {rule:?}"),
    }
}

fn mk_group_expr<F>(f: F, pairs: Pairs<pest_::Rule>) -> Expr
where
    F: FnOnce(Vec<Expr>) -> Expr,
{
    let es = pairs.map(parse_expr).collect::<Vec<_>>();

    if es.len() == 1 {
        es.into_iter().next().unwrap()
    } else {
        f(es)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Specifications {
    pub(crate) rules: Vec<Rule>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Rule {
    pub(crate) name: String,
    pub(crate) expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Seq(Vec<Expr>),
    Alt(Vec<Expr>),
    Rep(Box<Expr>, Quantifier),
    Token(Token),
    Rule(String),
    Fragment(Fragment),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Quantifier {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}
