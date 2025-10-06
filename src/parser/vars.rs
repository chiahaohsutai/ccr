use crate::tokens::Token;

/// Represents an integer constant in the AST.
pub struct Integer(Token);

impl From<Token> for Integer {
    fn from(token: Token) -> Self {
        match token {
            Token::CONSTANT(value) => Integer(Token::CONSTANT(value)),
            _ => panic!("Expected integer constant."),
        }
    }
}

/// Represents an identifier in the AST.
pub struct Identifier(Token);

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        match token {
            Token::IDENTIFIER(name) => Identifier(Token::IDENTIFIER(name)),
            _ => panic!("Expected identifier."),
        }
    }
}

/// Represents an expression in the AST.
pub enum Expression {
    INT(Integer),
}

/// Represents a statement in the AST.
pub enum Statement {
    RETURN(Expression),
}

/// Represents a function in the AST.
pub struct Function {
    name: Identifier,
    body: Statement, 
}

/// Methods for the Function struct.
impl Function {
    pub fn new(identifier: Identifier, statement: Statement) -> Self {
        Function { name: identifier, body: statement }
    }
}

/// Represents a complete program in the AST.
pub struct Program(Function);

impl From<Function> for Program {
    fn from(function: Function) -> Self {
        Program(function)
    }
}
