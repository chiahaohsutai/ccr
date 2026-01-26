use std::collections::{HashSet, VecDeque};

use super::generate_tag;
use super::tokenizer::Token;

mod blocks;
mod decls;
mod exprs;
mod factors;
mod stmts;

type ParserResult<T> = Result<(T, State), String>;
type Parser<T> = fn(State) -> ParserResult<T>;

trait TupleExt<T, U> {
    fn map_first<K, F>(self, f: F) -> (K, U)
    where
        F: FnOnce(T) -> K;
}

impl<T, U> TupleExt<T, U> for (T, U) {
    fn map_first<K, F>(self, f: F) -> (K, U)
    where
        F: FnOnce(T) -> K,
    {
        (f(self.0), self.1)
    }
}

fn consume_and_expect<T>(state: State, consumer: Parser<T>, expect: Token) -> ParserResult<T> {
    let (node, mut state) = consumer(state)?;
    match state.tokens.pop_front() {
        Some(token) if token.eq(&expect) => Ok((node, state)),
        Some(token) => Err(format!("Expected `{expect}` found `{token}`")),
        None => Err(format!("Unexpected end of input: expected {expect}")),
    }
}

enum Scope {
    Loop(String),
    Switch(String),
}

struct State {
    tokens: VecDeque<Token>,
    scopes: VecDeque<Scope>,
    labels: HashSet<String>,
}

fn generate_label() -> String {
    generate_tag("label")
}

pub struct Program(Vec<decls::FnDecl>);

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    todo!()
}
