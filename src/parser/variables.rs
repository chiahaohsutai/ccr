
use crate::tokens::Token;

pub struct Integer(Token);

impl From<Token> for Integer {
    fn from(token: Token) -> Self {
        match token {
            Token::CONSTANT(value) => Integer(Token::CONSTANT(value)),
            _ => panic!("Expected integer constant."),
        }
    }
}

pub struct Identifier(Token);

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        match token {
            Token::IDENTIFIER(name) => Identifier(Token::IDENTIFIER(name)),
            _ => panic!("Expected identifier."),
        }
    }
}

pub enum Expression {
    Int(Integer),
}

pub enum Statement {
    Return(Expression),
}

pub struct Function(Identifier, Statement);

impl Function {
    pub fn new(identifier: Identifier, statement: Statement) -> Self {
        Function(identifier, statement)
    }
}

pub struct Program(Function);

impl Program {
    pub fn new(function: Function) -> Self {
        Program(function)
    }
}