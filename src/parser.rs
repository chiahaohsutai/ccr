use super::tokens::{self, Token};
use core::fmt;
use std::collections::VecDeque;
use std::str::FromStr;
use tracing::info;

/// Represents an integer constant in the AST.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Integer(u64);

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Integer {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<u64>()
            .map(Integer)
            .map_err(|e| format!("Failed to parse integer '{}': {}", s, e))
    }
}

impl From<Integer> for u64 {
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
    NOT,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::NEGATE => write!(f, "-"),
            UnaryOperator::COMPLEMENT => write!(f, "~"),
            UnaryOperator::NOT => write!(f, "!"),
        }
    }
}

impl TryFrom<tokens::Operator> for UnaryOperator {
    type Error = String;

    fn try_from(op: tokens::Operator) -> Result<Self, Self::Error> {
        match op {
            tokens::Operator::NEGATION => Ok(UnaryOperator::NEGATE),
            tokens::Operator::COMPLEMENT => Ok(UnaryOperator::COMPLEMENT),
            tokens::Operator::NOT => Ok(UnaryOperator::NOT),
            _ => Err(format!("Operator '{}' is not a unary operator.", op)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    REMAINDER,
    BITWISEAND,
    BITWISEOR,
    BITWISEXOR,
    LEFTSHIFT,
    RIGHTSHIFT,
    AND,
    OR,
    EQUAL,
    NOTEQUAL,
    LESSTHAN,
    GREATERTHAN,
    LESSEQUAL,
    GREATEREQUAL,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::ADD => write!(f, "+"),
            BinaryOperator::SUBTRACT => write!(f, "-"),
            BinaryOperator::MULTIPLY => write!(f, "*"),
            BinaryOperator::DIVIDE => write!(f, "/"),
            BinaryOperator::REMAINDER => write!(f, "%"),
            BinaryOperator::BITWISEAND => write!(f, "&"),
            BinaryOperator::BITWISEOR => write!(f, "|"),
            BinaryOperator::BITWISEXOR => write!(f, "^"),
            BinaryOperator::LEFTSHIFT => write!(f, "<<"),
            BinaryOperator::RIGHTSHIFT => write!(f, ">>"),
            BinaryOperator::AND => write!(f, "&&"),
            BinaryOperator::OR => write!(f, "||"),
            BinaryOperator::EQUAL => write!(f, "=="),
            BinaryOperator::NOTEQUAL => write!(f, "!="),
            BinaryOperator::LESSTHAN => write!(f, "<"),
            BinaryOperator::GREATERTHAN => write!(f, ">"),
            BinaryOperator::LESSEQUAL => write!(f, "<="),
            BinaryOperator::GREATEREQUAL => write!(f, ">="),
        }
    }
}

impl TryFrom<tokens::Operator> for BinaryOperator {
    type Error = String;

    fn try_from(op: tokens::Operator) -> Result<Self, Self::Error> {
        match op {
            tokens::Operator::ADDITION => Ok(BinaryOperator::ADD),
            tokens::Operator::NEGATION => Ok(BinaryOperator::SUBTRACT),
            tokens::Operator::PRODUCT => Ok(BinaryOperator::MULTIPLY),
            tokens::Operator::DIVISION => Ok(BinaryOperator::DIVIDE),
            tokens::Operator::REMAINDER => Ok(BinaryOperator::REMAINDER),
            tokens::Operator::BITWISEAND => Ok(BinaryOperator::BITWISEAND),
            tokens::Operator::BITWISEOR => Ok(BinaryOperator::BITWISEOR),
            tokens::Operator::BITWISEXOR => Ok(BinaryOperator::BITWISEXOR),
            tokens::Operator::LEFTSHIFT => Ok(BinaryOperator::LEFTSHIFT),
            tokens::Operator::RIGHTSHIFT => Ok(BinaryOperator::RIGHTSHIFT),
            tokens::Operator::AND => Ok(BinaryOperator::AND),
            tokens::Operator::OR => Ok(BinaryOperator::OR),
            tokens::Operator::EQUAL => Ok(BinaryOperator::EQUAL),
            tokens::Operator::NOTEQUAL => Ok(BinaryOperator::NOTEQUAL),
            tokens::Operator::LESSTHAN => Ok(BinaryOperator::LESSTHAN),
            tokens::Operator::GREATERTHAN => Ok(BinaryOperator::GREATERTHAN),
            tokens::Operator::LESSEQUAL => Ok(BinaryOperator::LESSEQUAL),
            tokens::Operator::GREATEREQUAL => Ok(BinaryOperator::GREATEREQUAL),
            _ => Err(format!("Operator '{}' is not a binary operator.", op)),
        }
    }
}

impl TryFrom<Token> for BinaryOperator {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::OPERATOR(op) => BinaryOperator::try_from(op),
            _ => Err(String::from("Token is not an operator.")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor {
    INT(Integer),
    UNARY(UnaryOperator, Box<Factor>),
    EXPRESSION(Box<Expression>),
}

impl fmt::Display for Factor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Factor::INT(n) => write!(f, "{}", n),
            Factor::UNARY(op, factor) => write!(f, "{}{}", op, factor),
            Factor::EXPRESSION(expr) => write!(f, "({})", expr),
        }
    }
}

impl From<u64> for Factor {
    fn from(value: u64) -> Self {
        Factor::INT(Integer(value))
    }
}

/// Represents an expression in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    FACTOR(Factor),
    BINARY(Box<Expression>, BinaryOperator, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::FACTOR(factor) => write!(f, "{}", factor),
            Expression::BINARY(l, op, r) => write!(f, "{l} {op} {r}"),
        }
    }
}

impl From<u64> for Expression {
    fn from(value: u64) -> Self {
        Expression::FACTOR(Factor::from(value))
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
        writeln!(f, "FN {}\n\t{}\nEND FN {}", self.0, self.1, self.0)
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

fn parse_factor(tokens: &mut VecDeque<Token>) -> Result<Factor, String> {
    match tokens.pop_front() {
        Some(Token::CONSTANT(value)) => Ok(Factor::from(value)),
        Some(Token::OPERATOR(op)) => match op {
            tokens::Operator::NEGATION => {
                let factor = parse_factor(tokens)?;
                Ok(Factor::UNARY(UnaryOperator::NEGATE, Box::new(factor)))
            }
            tokens::Operator::COMPLEMENT => {
                let factor = parse_factor(tokens)?;
                Ok(Factor::UNARY(UnaryOperator::COMPLEMENT, Box::new(factor)))
            }
            tokens::Operator::NOT => {
                let factor = parse_factor(tokens)?;
                Ok(Factor::UNARY(UnaryOperator::NOT, Box::new(factor)))
            }
            _ => Err(format!("Unexpected operator '{}' in factor.", op)),
        },
        Some(Token::DELIMITER(tokens::Delimiter::LPAREN)) => {
            let expr = parse_expression(tokens, 0)?;
            if let Some(Token::DELIMITER(tokens::Delimiter::RPAREN)) = tokens.pop_front() {
                Ok(Factor::EXPRESSION(Box::new(expr)))
            } else {
                Err(String::from("Expected ')' after expression."))
            }
        }
        _ => Err(String::from("Malformed factor.")),
    }
}

/// Parses expression production rule.
fn parse_expression(tokens: &mut VecDeque<Token>, precedence: u64) -> Result<Expression, String> {
    let mut left = Expression::FACTOR(parse_factor(tokens)?);
    let mut next = tokens.pop_front();

    while next
        .as_ref()
        .is_some_and(|t| t.is_binary_operator() && t.precedence() >= precedence)
    {
        let token = next.unwrap();
        let precedence = token.precedence() + 1;
        let right = parse_expression(tokens, precedence)?;
        left = Expression::BINARY(
            Box::new(left),
            BinaryOperator::try_from(token).unwrap(),
            Box::new(right),
        );
        next = tokens.pop_front();
    }

    if let Some(t) = next {
        tokens.push_front(t);
    }
    Ok(left)
}

/// Parses statement production rule.
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, String> {
    match tokens.pop_front() {
        Some(Token::KEYWORD(tokens::Keyword::RETURN)) => {
            let expr = parse_expression(tokens, 0)?;
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
