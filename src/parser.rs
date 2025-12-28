use core::fmt;
use std::collections::VecDeque;
use tracing::info;

use super::tokenizer::{self, Token};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    NOT,
    NEGATE,
    COMPLEMENT,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::NOT => write!(f, "!"),
            UnaryOperator::NEGATE => write!(f, "-"),
            UnaryOperator::COMPLEMENT => write!(f, "~"),
        }
    }
}

impl TryFrom<tokenizer::Operator> for UnaryOperator {
    type Error = String;

    fn try_from(op: tokenizer::Operator) -> Result<Self, Self::Error> {
        match op {
            tokenizer::Operator::NEGATION => Ok(UnaryOperator::NEGATE),
            tokenizer::Operator::COMPLEMENT => Ok(UnaryOperator::COMPLEMENT),
            tokenizer::Operator::NOT => Ok(UnaryOperator::NOT),
            _ => Err(format!("Operator '{op}' is not a unary operator.")),
        }
    }
}

impl TryFrom<Token> for UnaryOperator {
    type Error = String;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::OPERATOR(op) => UnaryOperator::try_from(op),
            _ => Err(String::from("Token is not an operator.")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    ADD,
    DIVIDE,
    SUBTRACT,
    MULTIPLY,
    REMAINDER,
    BITWISEOR,
    BITWISEAND,
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
    ASSIGNMENT,
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
            BinaryOperator::ASSIGNMENT => write!(f, "="),
        }
    }
}

impl TryFrom<tokenizer::Operator> for BinaryOperator {
    type Error = String;

    fn try_from(op: tokenizer::Operator) -> Result<Self, Self::Error> {
        match op {
            tokenizer::Operator::ADDITION => Ok(BinaryOperator::ADD),
            tokenizer::Operator::NEGATION => Ok(BinaryOperator::SUBTRACT),
            tokenizer::Operator::PRODUCT => Ok(BinaryOperator::MULTIPLY),
            tokenizer::Operator::DIVISION => Ok(BinaryOperator::DIVIDE),
            tokenizer::Operator::REMAINDER => Ok(BinaryOperator::REMAINDER),
            tokenizer::Operator::BITWISEAND => Ok(BinaryOperator::BITWISEAND),
            tokenizer::Operator::BITWISEOR => Ok(BinaryOperator::BITWISEOR),
            tokenizer::Operator::BITWISEXOR => Ok(BinaryOperator::BITWISEXOR),
            tokenizer::Operator::LEFTSHIFT => Ok(BinaryOperator::LEFTSHIFT),
            tokenizer::Operator::RIGHTSHIFT => Ok(BinaryOperator::RIGHTSHIFT),
            tokenizer::Operator::AND => Ok(BinaryOperator::AND),
            tokenizer::Operator::OR => Ok(BinaryOperator::OR),
            tokenizer::Operator::EQUAL => Ok(BinaryOperator::EQUAL),
            tokenizer::Operator::NOTEQUAL => Ok(BinaryOperator::NOTEQUAL),
            tokenizer::Operator::LESSTHAN => Ok(BinaryOperator::LESSTHAN),
            tokenizer::Operator::GREATERTHAN => Ok(BinaryOperator::GREATERTHAN),
            tokenizer::Operator::LESSEQUAL => Ok(BinaryOperator::LESSEQUAL),
            tokenizer::Operator::GREATEREQUAL => Ok(BinaryOperator::GREATEREQUAL),
            tokenizer::Operator::ASSIGNMENT => Ok(BinaryOperator::ASSIGNMENT),
            _ => Err(format!("Operator '{op}' is not a binary operator.")),
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
    INT(u64),
    IDENTIFIER(String),
    UNARY(UnaryOperator, Box<Factor>),
    EXPRESSION(Box<Expression>),
}

impl fmt::Display for Factor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Factor::INT(n) => write!(f, "{n}"),
            Factor::UNARY(op, fac) => write!(f, "{op}{fac}"),
            Factor::EXPRESSION(e) => write!(f, "({e})"),
            Factor::IDENTIFIER(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    FACTOR(Factor),
    BINARY(Box<Expression>, BinaryOperator, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::FACTOR(fac) => write!(f, "{fac}"),
            Expression::BINARY(l, op, r) => write!(f, "{l} {op} {r}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration(String, Option<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    NULL,
    RETURN(Expression),
    EXPRESSION(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::NULL => write!(f, ";"),
            Statement::EXPRESSION(expr) => write!(f, "{};", expr),
            Statement::RETURN(expr) => write!(f, "RETURN {};", expr),
        }
    }
}

// Represents a block item in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

impl fmt::Display for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockItem::Declaration(decl) => write!(f, "{}", decl.0),
            BlockItem::Statement(stmt) => write!(f, "{}", stmt),
        }
    }
}

/// Represents a function in the AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Function(String, Vec<BlockItem>);

impl Function {
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let body = self
            .1
            .iter()
            .map(|i| format!("\t{}", i))
            .collect::<Vec<String>>();
        write!(f, "FN {}\n{}\nEND FN {}", self.0, body.join("\n"), self.0)
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
        Some(Token::IDENTIFIER(name)) => Ok(Factor::IDENTIFIER(name)),
        Some(Token::CONSTANT(value)) => Ok(Factor::INT(value)),
        Some(Token::OPERATOR(op)) => match op {
            tokenizer::Operator::NEGATION => {
                let factor = parse_factor(tokens)?;
                Ok(Factor::UNARY(UnaryOperator::NEGATE, Box::new(factor)))
            }
            tokenizer::Operator::COMPLEMENT => {
                let factor = parse_factor(tokens)?;
                Ok(Factor::UNARY(UnaryOperator::COMPLEMENT, Box::new(factor)))
            }
            tokenizer::Operator::NOT => {
                let factor = parse_factor(tokens)?;
                Ok(Factor::UNARY(UnaryOperator::NOT, Box::new(factor)))
            }
            _ => Err(format!("Unexpected operator '{}' in factor.", op)),
        },
        Some(Token::DELIMITER(tokenizer::Delimiter::LPAREN)) => {
            let expr = parse_expression(tokens, 0)?;
            if let Some(Token::DELIMITER(tokenizer::Delimiter::RPAREN)) = tokens.pop_front() {
                Ok(Factor::EXPRESSION(Box::new(expr)))
            } else {
                Err(String::from("Expected ')' after expression."))
            }
        }
        tkn => Err(format!("Malformed factor, found {tkn:?}.")),
    }
}

/// Parses expression production rule with precedence climbing.
fn parse_expression(tokens: &mut VecDeque<Token>, precedence: u64) -> Result<Expression, String> {
    let mut left = Expression::FACTOR(parse_factor(tokens)?);
    let mut next = tokens.pop_front();

    while next
        .as_ref()
        .is_some_and(|t| t.is_binary_operator() && t.precedence() >= precedence)
    {
        let token = next.unwrap();
        let curr_precedence = token.precedence();

        if let Token::OPERATOR(tokenizer::Operator::ASSIGNMENT) = token {
            let right = parse_expression(tokens, curr_precedence)?;
            left = Expression::BINARY(Box::new(left), BinaryOperator::ASSIGNMENT, Box::new(right));
        } else {
            let right = parse_expression(tokens, curr_precedence + 1)?;
            left = Expression::BINARY(
                Box::new(left),
                BinaryOperator::try_from(token).unwrap(),
                Box::new(right),
            );
        }
        next = tokens.pop_front();
    }
    if let Some(t) = next {
        tokens.push_front(t);
    }
    Ok(left)
}

/// Parses block item production rule.
fn parse_block_item(tokens: &mut VecDeque<Token>) -> Result<BlockItem, String> {
    match tokens.pop_front() {
        Some(Token::DELIMITER(tokenizer::Delimiter::SEMICOLON)) => {
            Ok(BlockItem::Statement(Statement::NULL))
        }
        Some(Token::KEYWORD(tokenizer::Keyword::INT)) => {
            let name = match tokens.pop_front() {
                Some(Token::IDENTIFIER(id)) => Ok(id),
                None => Err(String::from("Unexpected end of input.")),
                _ => Err(String::from("Expected identifier after 'int'.")),
            }?;
            match tokens.pop_front() {
                Some(Token::DELIMITER(tokenizer::Delimiter::SEMICOLON)) => {
                    Ok(BlockItem::Declaration(Declaration(name, None)))
                }
                Some(Token::OPERATOR(tokenizer::Operator::ASSIGNMENT)) => {
                    let expr = parse_expression(tokens, 0)?;
                    match tokens.pop_front() {
                        Some(Token::DELIMITER(tokenizer::Delimiter::SEMICOLON)) => {
                            Ok(BlockItem::Declaration(Declaration(name, Some(expr))))
                        }
                        _ => Err(String::from("Expected ';' after declaration.")),
                    }
                }
                _ => Err(String::from("Expected ';' or '=' after variable name.")),
            }
        }
        Some(Token::KEYWORD(tokenizer::Keyword::RETURN)) => {
            let expr = parse_expression(tokens, 0)?;
            let stmt = match tokens.pop_front() {
                Some(Token::DELIMITER(tokenizer::Delimiter::SEMICOLON)) => {
                    Ok(Statement::RETURN(expr))
                }
                _ => Err(String::from("Expected ';' after return expression.")),
            };
            stmt.map(BlockItem::Statement)
        }
        Some(token) => {
            tokens.push_front(token);
            let expr = parse_expression(tokens, 0)?;
            match tokens.pop_front() {
                Some(Token::DELIMITER(tokenizer::Delimiter::SEMICOLON)) => {
                    Ok(BlockItem::Statement(Statement::EXPRESSION(expr)))
                }
                _ => Err(String::from("Expected ';' after expression statement.")),
            }
        }
        None => Err(String::from("Unexpected end of input while parsing body.")),
    }
}

/// Parses function production rule.
fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function, String> {
    match tokens.pop_front() {
        Some(Token::KEYWORD(tokenizer::Keyword::INT)) => {
            let name = match tokens.pop_front() {
                Some(Token::IDENTIFIER(name)) => Ok(name),
                None => Err(String::from("Unexpected end of input.")),
                _ => Err(String::from("Expected identifier.")),
            }?;
            match (tokens.pop_front(), tokens.pop_front(), tokens.pop_front()) {
                (
                    Some(Token::DELIMITER(tokenizer::Delimiter::LPAREN)),
                    Some(Token::KEYWORD(tokenizer::Keyword::VOID)),
                    Some(Token::DELIMITER(tokenizer::Delimiter::RPAREN)),
                ) => match tokens.pop_front() {
                    Some(Token::DELIMITER(tokenizer::Delimiter::LBRACE)) => {
                        let mut block: Vec<BlockItem> = Vec::new();
                        loop {
                            let next = tokens.front();
                            if let Some(Token::DELIMITER(tokenizer::Delimiter::RBRACE)) = next {
                                tokens.pop_front();
                                break Ok(Function(name, block));
                            }
                            let item = parse_block_item(tokens)?;
                            block.push(item);
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
