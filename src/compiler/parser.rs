use std::collections::{HashSet, VecDeque};

use super::generate_tag;
use super::tokenizer::Token;

mod exprs;
mod factors;

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
        Some(token) => Err(format!("Expected '{expect}' found '{token}'")),
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

enum Decl {
    FnDecl(FnDecl),
    VarDecl(VarDecl),
}

struct VarDecl {
    name: String,
    expr: Option<exprs::Expr>,
}

struct FnDecl {
    name: String,
    params: Vec<String>,
    body: Block,
}

enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

struct Block {
    items: Vec<BlockItem>,
}

enum ForInit {
    Decl(Decl),
    Expr(Option<exprs::Expr>),
}

struct If {
    cond: exprs::Expr,
    body: Box<Self>,
    otherwise: Option<Box<Self>>,
}

struct Label {
    name: String,
    body: Box<Stmt>,
}

struct While {
    id: String,
    cond: exprs::Expr,
    body: Box<Stmt>,
}

enum Case {
    Int(u64, String),
    Default(String),
}

struct Switch {
    id: String,
    value: exprs::Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

struct Clause {
    parent: String,
    value: exprs::Expr,
    body: Box<Stmt>,
}

struct Default {
    parent: String,
    body: Box<Stmt>,
}

struct For {
    id: String,
    init: ForInit,
    cond: Option<exprs::Expr>,
    post: Option<exprs::Expr>,
    body: Box<Stmt>,
}

enum Stmt {
    Null,
    Return(exprs::Expr),
    Expr(exprs::Expr),
    If(If),
    Goto(String),
    Label(Label),
    Comp(Block),
    Break(String),
    Continue(String),
    While(While),
    DoWhile(While),
    Switch(Switch),
    Case(Clause),
    Default(Default),
    For(For),
}

struct Program(Vec<FnDecl>);

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    todo!()
}
