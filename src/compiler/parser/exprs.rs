use super::super::tokenizer::Token;
use super::{ParserResult, State, TupleExt, consume_and_expect, factors};

type ExprResult = ParserResult<Expr>;

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
    Lt,
    Gt,
    Le,
    Ge,
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    MulAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
}

impl BinOp {
    fn precedence(&self) -> u64 {
        match self {
            Self::Assign
            | Self::SubAssign
            | Self::DivAssign
            | Self::AddAssign
            | Self::RemAssign
            | Self::MulAssign
            | Self::AndAssign
            | Self::OrAssign
            | Self::XorAssign
            | Self::LShiftAssign
            | Self::RShiftAssign => 1,
            Self::Or => 5,
            Self::And => 10,
            Self::BitOr => 15,
            Self::BitXor => 20,
            Self::BitAnd => 25,
            Self::EqEq | Self::NotEq => 30,
            Self::Lt | Self::Gt | Self::Le | Self::Ge => 35,
            Self::RShift | Self::LShift => 40,
            Self::Add | Self::Sub => 45,
            Self::Mul | Self::Div | Self::Rem => 50,
        }
    }
}

impl TryFrom<&Token> for BinOp {
    type Error = String;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Star => Ok(Self::Mul),
            Token::Slash => Ok(Self::Div),
            Token::Percent => Ok(Self::Rem),
            Token::Amp => Ok(Self::BitAnd),
            Token::Pipe => Ok(Self::BitOr),
            Token::Caret => Ok(Self::BitXor),
            Token::Shl => Ok(Self::LShift),
            Token::Shr => Ok(Self::RShift),
            Token::AmpAmp => Ok(Self::And),
            Token::PipePipe => Ok(Self::Or),
            Token::EqEq => Ok(Self::EqEq),
            Token::NotEq => Ok(Self::NotEq),
            Token::Lt => Ok(Self::Lt),
            Token::Gt => Ok(Self::Gt),
            Token::Le => Ok(Self::Le),
            Token::Ge => Ok(Self::Ge),
            Token::Eq => Ok(Self::Assign),
            Token::PlusEq => Ok(Self::AddAssign),
            Token::MinusEq => Ok(Self::SubAssign),
            Token::SlashEq => Ok(Self::DivAssign),
            Token::PercentEq => Ok(Self::RemAssign),
            Token::StarEq => Ok(Self::MulAssign),
            Token::AmpEq => Ok(Self::AndAssign),
            Token::PipeEq => Ok(Self::OrAssign),
            Token::CaretEq => Ok(Self::XorAssign),
            Token::ShlEq => Ok(Self::LShiftAssign),
            Token::ShrEq => Ok(Self::RShiftAssign),
            token => Err(format!("Expected binary operator found: {token}")),
        }
    }
}

impl TryFrom<Option<&Token>> for BinOp {
    type Error = String;

    fn try_from(value: Option<&Token>) -> Result<Self, Self::Error> {
        if let Some(token) = value {
            Self::try_from(token)
        } else {
            Err(format!("Expected binary operator"))
        }
    }
}

struct Ternary {
    cond: Box<Expr>,
    then: Box<Expr>,
    otherwise: Box<Expr>,
}

impl Ternary {
    fn new(cond: Expr, then: Expr, otherwise: Expr) -> Self {
        Self {
            cond: Box::new(cond),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    }
}

struct BinExpr {
    lhs: Box<Expr>,
    op: BinOp,
    rhs: Box<Expr>,
}

impl BinExpr {
    #[rustfmt::skip]
    fn new(lhs: Expr, op: BinOp, rhs: Expr) -> Self {
        Self { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
    }
}

pub enum Expr {
    Fac(factors::Factor),
    Tern(Ternary),
    Bin(BinExpr),
}

impl From<factors::Factor> for Expr {
    fn from(value: factors::Factor) -> Self {
        Self::Fac(value)
    }
}

fn is_assignment(op: &BinOp) -> bool {
    matches!(
        op,
        BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::DivAssign
            | BinOp::RemAssign
            | BinOp::MulAssign
            | BinOp::AndAssign
            | BinOp::OrAssign
            | BinOp::XorAssign
            | BinOp::RShiftAssign
            | BinOp::LShiftAssign
    )
}

enum Op {
    Ternary,
    BinOp(BinOp),
}

impl Op {
    fn precedence(&self) -> u64 {
        match self {
            Self::BinOp(op) => op.precedence(),
            _ => 0,
        }
    }
}

impl TryFrom<&Token> for Op {
    type Error = String;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Eroteme => Ok(Self::Ternary),
            token => BinOp::try_from(token).map(|op| Self::BinOp(op)),
        }
    }
}

fn consume_token(mut state: State) -> ParserResult<Token> {
    let token = state
        .tokens
        .pop_front()
        .ok_or(String::from("Unexpected end of input: expected expression"))?;
    Ok((token, state))
}

fn consume_assignment(state: State, operand: Expr, precedence: u64) -> ExprResult {
    let (token, state) = consume_token(state)?;
    let op = BinOp::try_from(&token)?;
    let (expr, state) = consume_and_climb(state, op.precedence())?;
    Ok((Expr::Bin(BinExpr::new(operand, op, expr)), state))
}

fn consume_ternary(state: State, operand: Expr, precedence: u64) -> ExprResult {
    let (token, state) = consume_token(state)?;
    if matches!(token, Token::Eroteme) {
        let (then, mut state) = consume_and_climb(state, 0)?;
        let (otherwise, state) = match state.tokens.pop_front() {
            Some(Token::Colon) => consume_and_climb(state, 0)?,
            _ => return Err(format!("Expected `:` found: {token}")),
        };
        Ok((Expr::Tern(Ternary::new(operand, then, otherwise)), state))
    } else {
        Err(format!("Expected `?` found: {token}"))
    }
}

fn consume_expr(mut state: State, operand: Expr) -> ExprResult {
    let (token, state) = consume_token(state)?;
    let op = BinOp::try_from(&token)?;
    let (expr, state) = consume_and_climb(state, op.precedence() + 1)?;
    Ok((Expr::Bin(BinExpr::new(operand, op, expr)), state))
}

fn consume_and_climb(state: State, precedence: u64) -> ExprResult {
    let (mut lhs, mut state) = factors::parse(state)?.map_first(Expr::from);
    let mut op = state.tokens.front().map(|t| Op::try_from(t).ok()).flatten();

    while op.as_ref().is_some_and(|op| op.precedence() >= precedence) {
        (lhs, state) = match op.as_ref().unwrap() {
            Op::Ternary => consume_ternary(state, lhs, precedence)?,
            Op::BinOp(op) if is_assignment(&op) => consume_assignment(state, lhs, precedence)?,
            Op::BinOp(_) => consume_expr(state, lhs)?,
        };
        op = state.tokens.front().map(|t| Op::try_from(t).ok()).flatten()
    }
    Ok((lhs.into(), state))
}

pub fn parse(state: State) -> ExprResult {
    consume_and_climb(state, 0)
}
