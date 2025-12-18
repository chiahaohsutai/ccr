use super::tokens::{self, Token};
use core::fmt;
use std::collections::VecDeque;
use std::str::FromStr;
use tracing::info;

/// Represents an integer constant in the AST.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Integer(i64);

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Integer {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<i64>()
            .map(Integer)
            .map_err(|e| format!("Failed to parse integer '{}': {}", s, e))
    }
}

impl From<Integer> for i64 {
    fn from(integer: Integer) -> Self {
        integer.0
    }
}

/// Represents an identifier in the AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for Identifier {
    fn from(name: &str) -> Self {
        Identifier(String::from(name))
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Represents unary operators in the AST.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    NEGATE,
    COMPLEMENT,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::NEGATE => write!(f, "-"),
            UnaryOperator::COMPLEMENT => write!(f, "~"),
        }
    }
}

/// Represents an expression in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    INT(Integer),
    UNARY(UnaryOperator, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::INT(n) => write!(f, "{}", n),
            Expression::UNARY(op, expr) => write!(f, "{}{}", op, expr),
        }
    }
}

impl From<i64> for Expression {
    fn from(value: i64) -> Self {
        Expression::INT(Integer(value))
    }
}

/// Represents a statement in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    RETURN(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::RETURN(expr) => write!(f, "RETURN {};", expr),
        }
    }
}

impl From<Function> for Statement {
    fn from(function: Function) -> Self {
        function.1
    }
}

/// Represents a function in the AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Function(Identifier, Statement);

impl Function {
    pub fn new(name: Identifier, body: Statement) -> Self {
        Function(name, body)
    }
    pub fn name(&self) -> &Identifier {
        &self.0
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FUNCTION {}", self.0)?;
        writeln!(f, "  {}", self.1)?;
        writeln!(f, "END FUNCTION")
    }
}

impl From<Program> for Function {
    fn from(program: Program) -> Self {
        program.0
    }
}

/// Represents a complete program in the AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Program(Function);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Parses expression production rule.
fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Expression, String> {
    match tokens.pop_front() {
        Some(Token::CONSTANT(n)) => Ok(Expression::from(n)),
        Some(Token::OPERATOR(op)) => {
            let expr = parse_expression(tokens)?;
            let op = match op {
                tokens::Operator::NEGATION => Ok(UnaryOperator::NEGATE),
                tokens::Operator::BITWISENOT => Ok(UnaryOperator::COMPLEMENT),
                tokens::Operator::DECREMENT => {
                    Err(String::from("Decrement operator not implemented"))
                }
            };
            Ok(Expression::UNARY(op?, Box::new(expr)))
        }
        Some(Token::DELIMITER(tokens::Delimiter::LPAREN)) => {
            let expr = parse_expression(tokens);
            match tokens.pop_front() {
                Some(Token::DELIMITER(tokens::Delimiter::RPAREN)) => expr,
                _ => Err(String::from("Expected ')' after expression.")),
            }
        }
        _ => Err(String::from("Malformed expression.")),
    }
}

/// Parses statement production rule.
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, String> {
    match tokens.pop_front() {
        Some(Token::KEYWORD(tokens::Keyword::RETURN)) => {
            let expr = parse_expression(tokens)?;
            match tokens.pop_front() {
                Some(Token::DELIMITER(tokens::Delimiter::SEMICOLON)) => Ok(Statement::RETURN(expr)),
                _ => Err(String::from("Expected ';' after return expression.")),
            }
        }
        None => Err(String::from("Unexpected end of input while parsing.")),
        _ => Err(String::from("Expected statement.")),
    }
}

/// Parses identifier production rule.
fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<Identifier, String> {
    match tokens.pop_front() {
        Some(Token::IDENTIFIER(name)) => Ok(Identifier(name)),
        None => Err(String::from("Unexpected end of input.")),
        _ => Err(String::from("Expected identifier.")),
    }
}

/// Parses function production rule.
fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function, String> {
    match tokens.pop_front() {
        Some(Token::KEYWORD(tokens::Keyword::INT)) => {
            let ident = parse_identifier(tokens)?;
            match (tokens.pop_front(), tokens.pop_front(), tokens.pop_front()) {
                (
                    Some(Token::DELIMITER(tokens::Delimiter::LPAREN)),
                    Some(Token::KEYWORD(tokens::Keyword::VOID)),
                    Some(Token::DELIMITER(tokens::Delimiter::RPAREN)),
                ) => match tokens.pop_front() {
                    Some(Token::DELIMITER(tokens::Delimiter::LBRACE)) => {
                        let stmt = parse_statement(tokens)?;
                        match tokens.pop_front() {
                            Some(Token::DELIMITER(tokens::Delimiter::RBRACE)) => {
                                Ok(Function::new(ident, stmt))
                            }
                            _ => Err(String::from("Expected '}' at the end of fn body.")),
                        }
                    }
                    _ => Err(String::from("Expected '{' at the start of fn body.")),
                },
                _ => Err(String::from("Expected '(void)' after fn name.")),
            }
        }
        None => Err(String::from("Unexpected end of input while parsing fn.")),
        _ => Err(String::from("Expected 'int' keyword at the start of a fn.")),
    }
}

/// Parses program production rule.
pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    info!("Generating AST...");

    let mut tokens: VecDeque<Token> = VecDeque::from(tokens);
    let program = Program(parse_function(&mut tokens)?);

    if tokens.is_empty() {
        Ok(program)
    } else {
        Err(String::from("Unexpected token after program end."))
    }
}
