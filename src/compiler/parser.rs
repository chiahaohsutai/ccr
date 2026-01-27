use std::collections::{HashSet, VecDeque};

use super::generate_tag;
use super::tokenizer::Token;

mod blocks;
mod decls;
mod exprs;
mod factors;
mod stmts;

type ParserResult<T> = Result<(T, State), String>;

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
