use std::collections::VecDeque;
use std::fmt;

use super::generate_tag;
use super::tokenizer::Token;

enum Scope {
    Loop(String),
    Switch(String),
}

struct State {
    tokens: VecDeque<Token>,
    scopes: VecDeque<Scope>,
}

type ParserResult<T> = Result<(T, State), String>;
type Parser<T> = fn(State) -> ParserResult<T>;

fn generate_label() -> String {
    generate_tag("label")
}

fn expect(mut state: State, expected: Token) -> Result<State, String> {
    let token = state
        .tokens
        .pop_front()
        .ok_or(format!("Unexpected end of input: expected `{expected}`"))?;
    token
        .eq(&expected)
        .then(|| state)
        .ok_or(format!("Expected `{expected}` but found `{token}`"))
}

fn then_expect<T, K>(state: State, handler: Parser<K>, expected: Token) -> ParserResult<T>
where
    T: From<K>,
{
    let (node, state) = handler(state)?;
    let state = expect(state, expected)?;
    Ok((node.into(), state))
}

enum UnaryOp {
    Decr,
    Incr,
    Neg,
    Not,
    Compl,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Decr => write!(f, "--"),
            Self::Incr => write!(f, "++"),
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Compl => write!(f, "~"),
        }
    }
}

enum BinOp {
    Add,
    Div,
    Sub,
    Mul,
    Rem,
    BitOr,
    BitAnd,
    BitXor,
    LShift,
    RShift,
    And,
    Or,
    EqEq,
    NotEq,
    LT,
    GT,
    LTE,
    GTE,
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    ProdAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
}

enum Factor {
    Int(u64),
    Ident(String),
    Expr(Box<Expr>),
    PreUnary(UnaryOp, Box<Self>),
    PosUnary(UnaryOp, Box<Self>),
}

impl From<Expr> for Factor {
    fn from(value: Expr) -> Self {
        Self::Expr(Box::new(value))
    }
}

fn parse_prefix_unary_factor(op: UnaryOp, state: State) -> ParserResult<Factor> {
    let (factor, state) = parse_factor(state)?;
    Ok((Factor::PreUnary(op, Box::new(factor)), state))
}

fn parse_ident_factor(ident: String, mut state: State) -> ParserResult<Factor> {
    let next = state.tokens.front();
    todo!()
    // Token::Ident(ident) if next_is_incr_decr => {
    //         let factor = Box::new(Factor::Ident(ident));
    //         if matches!(state.tokens.pop_front().unwrap(), Token::PlusPlus) {
    //             (Factor::PosUnary(UnaryOp::Incr, factor), state)
    //         } else {
    //             (Factor::PosUnary(UnaryOp::Decr, factor), state)
    //         }
    //     }
    //     Token
    //     Token::Ident(ident) => (Factor::Ident(ident), state),
}

fn parse_factor(mut state: State) -> ParserResult<Factor> {
    let curr = state.tokens.pop_front();
    let token = curr.ok_or("Unexpected end of input: expected factor")?;

    let (factor, state) = match token {
        Token::Const(constant) => (Factor::Int(constant), state),
        Token::Ident(ident) => parse_ident_factor(ident, state)?,
        Token::MinusMinus => parse_prefix_unary_factor(UnaryOp::Decr, state)?,
        Token::PlusPlus => parse_prefix_unary_factor(UnaryOp::Incr, state)?,
        Token::Minus => parse_prefix_unary_factor(UnaryOp::Neg, state)?,
        Token::Bang => parse_prefix_unary_factor(UnaryOp::Not, state)?,
        Token::Tilde => parse_prefix_unary_factor(UnaryOp::Compl, state)?,
        Token::LParen => then_expect(state, parse_expr, Token::RParen)?,
        token => return Err(format!("Invalid factor: unexpected token `{token}`")),
    };
    Ok((factor, state))
}

enum Expr {
    Fac(Factor),
    Cond(Box<Self>, Box<Self>, Box<Self>),
    Bin(Box<Self>, BinOp, Box<Self>),
}

fn parse_expr(mut state: State) -> ParserResult<Expr> {
    todo!()
}

enum Decl {
    FnDecl(FnDecl),
    VarDecl(VarDecl),
}

struct VarDecl {
    name: String,
    expr: Option<Expr>,
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
    Expr(Option<Expr>),
}

struct If {
    cond: Expr,
    body: Box<Self>,
    otherwise: Option<Box<Self>>,
}

struct Label {
    name: String,
    body: Box<Stmt>,
}

struct While {
    id: String,
    cond: Expr,
    body: Box<Stmt>,
}

enum Case {
    Int(u64, String),
    Default(String),
}

struct Switch {
    id: String,
    value: Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

struct Clause {
    parent: String,
    value: Expr,
    body: Box<Stmt>,
}

struct Default {
    parent: String,
    body: Box<Stmt>,
}

struct For {
    id: String,
    init: ForInit,
    cond: Option<Expr>,
    post: Option<Expr>,
    body: Box<Stmt>,
}

enum Stmt {
    Null,
    Return(Expr),
    Expr(Expr),
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
