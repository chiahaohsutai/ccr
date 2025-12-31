use core::fmt;
use std::collections::{HashMap, VecDeque};

use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;

use super::tokenizer;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    NOT,
    NEGATE,
    COMPLEMENT,
}

impl TryFrom<tokenizer::Operator> for UnaryOperator {
    type Error = String;

    fn try_from(op: tokenizer::Operator) -> Result<Self, Self::Error> {
        match op {
            tokenizer::Operator::Negation => Ok(Self::NEGATE),
            tokenizer::Operator::Complement => Ok(Self::COMPLEMENT),
            tokenizer::Operator::LogicalNot => Ok(Self::NOT),
            _ => Err(format!("Operator '{op}' is not a unary operator.")),
        }
    }
}

impl TryFrom<tokenizer::Token> for UnaryOperator {
    type Error = String;

    fn try_from(token: tokenizer::Token) -> Result<Self, Self::Error> {
        match token {
            tokenizer::Token::Operator(op) => Self::try_from(op),
            _ => Err(String::from("Token is not an operator.")),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NOT => write!(f, "!"),
            Self::NEGATE => write!(f, "-"),
            Self::COMPLEMENT => write!(f, "~"),
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

impl TryFrom<tokenizer::Operator> for BinaryOperator {
    type Error = String;

    fn try_from(op: tokenizer::Operator) -> Result<Self, Self::Error> {
        match op {
            tokenizer::Operator::Addition => Ok(Self::ADD),
            tokenizer::Operator::Negation => Ok(Self::SUBTRACT),
            tokenizer::Operator::Product => Ok(Self::MULTIPLY),
            tokenizer::Operator::Division => Ok(Self::DIVIDE),
            tokenizer::Operator::Remainder => Ok(Self::REMAINDER),
            tokenizer::Operator::BitwiseAnd => Ok(Self::BITWISEAND),
            tokenizer::Operator::BitwiseOr => Ok(Self::BITWISEOR),
            tokenizer::Operator::BitwiseXor => Ok(Self::BITWISEXOR),
            tokenizer::Operator::LeftShift => Ok(Self::LEFTSHIFT),
            tokenizer::Operator::RightShift => Ok(Self::RIGHTSHIFT),
            tokenizer::Operator::LogicalAnd => Ok(Self::AND),
            tokenizer::Operator::LogicalOr => Ok(Self::OR),
            tokenizer::Operator::EqualEqual => Ok(Self::EQUAL),
            tokenizer::Operator::NotEqual => Ok(Self::NOTEQUAL),
            tokenizer::Operator::LessThan => Ok(Self::LESSTHAN),
            tokenizer::Operator::GreaterThan => Ok(Self::GREATERTHAN),
            tokenizer::Operator::LessThanOrEqual => Ok(Self::LESSEQUAL),
            tokenizer::Operator::GreaterThanOrEqual => Ok(Self::GREATEREQUAL),
            tokenizer::Operator::Assignment => Ok(Self::ASSIGNMENT),
            _ => Err(format!("Operator '{op}' is not a binary operator.")),
        }
    }
}

impl TryFrom<tokenizer::Token> for BinaryOperator {
    type Error = String;

    fn try_from(token: tokenizer::Token) -> Result<Self, Self::Error> {
        match token {
            tokenizer::Token::Operator(op) => Self::try_from(op),
            _ => Err(String::from("Token is not an operator.")),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ADD => write!(f, "+"),
            Self::SUBTRACT => write!(f, "-"),
            Self::MULTIPLY => write!(f, "*"),
            Self::DIVIDE => write!(f, "/"),
            Self::REMAINDER => write!(f, "%"),
            Self::BITWISEAND => write!(f, "&"),
            Self::BITWISEOR => write!(f, "|"),
            Self::BITWISEXOR => write!(f, "^"),
            Self::LEFTSHIFT => write!(f, "<<"),
            Self::RIGHTSHIFT => write!(f, ">>"),
            Self::AND => write!(f, "&&"),
            Self::OR => write!(f, "||"),
            Self::EQUAL => write!(f, "=="),
            Self::NOTEQUAL => write!(f, "!="),
            Self::LESSTHAN => write!(f, "<"),
            Self::GREATERTHAN => write!(f, ">"),
            Self::LESSEQUAL => write!(f, "<="),
            Self::GREATEREQUAL => write!(f, ">="),
            Self::ASSIGNMENT => write!(f, "="),
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

impl Factor {
    fn is_identifier(&self) -> bool {
        matches!(self, Self::IDENTIFIER(_))
    }

    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Identifier(name)) => Ok(Self::IDENTIFIER(name)),
            Some(tokenizer::Token::Constant(value)) => Ok(Self::INT(value)),
            Some(tokenizer::Token::Operator(op)) => {
                let factor = Box::new(Self::parse(tokens)?);
                UnaryOperator::try_from(op)
                    .map_err(|_| format!("Unexpected unary operator '{}' in factor.", op))
                    .map(|op| Self::UNARY(op, factor))
            }
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                let expr = Expression::parse(tokens, 0)?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                        Ok(Self::EXPRESSION(Box::new(expr)))
                    }
                    _ => Err(String::from("Expected ')' after expression.")),
                }
            }
            tok => Err(format!("Malformed factor, found {tok:?}.")),
        }
    }

    fn resolve(factor: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        match factor {
            Self::IDENTIFIER(ident) => match variables.get(&ident) {
                Some(ident) => Ok(Self::IDENTIFIER(String::from(ident))),
                None => Err(String::from("Undeclared variable")),
            },
            Self::EXPRESSION(expr) => {
                let expr = Box::new(Expression::resolve(*expr, variables)?);
                Ok(Self::EXPRESSION(expr))
            }
            Self::UNARY(op, factor) => {
                let factor = Box::new(Factor::resolve(*factor, variables)?);
                Ok(Self::UNARY(op, factor))
            }
            factor => Ok(factor),
        }
    }
}

impl fmt::Display for Factor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::INT(n) => write!(f, "{n}"),
            Self::UNARY(op, fac) => write!(f, "{op}{fac}"),
            Self::EXPRESSION(e) => write!(f, "({e})"),
            Self::IDENTIFIER(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    FACTOR(Factor),
    BINARY(Box<Expression>, BinaryOperator, Box<Expression>),
}

impl Expression {
    fn is_identifier(&self) -> bool {
        match self {
            Self::FACTOR(f) if f.is_identifier() => true,
            _ => false,
        }
    }

    fn parse(tokens: &mut VecDeque<tokenizer::Token>, precedence: u64) -> Result<Self, String> {
        let mut lhs = Expression::FACTOR(Factor::parse(tokens)?);

        while tokens
            .front()
            .is_some_and(|t| t.is_binary_operator() && t.precedence() >= precedence)
        {
            let token = tokens.pop_front().unwrap();
            let precedence = token.precedence();

            if let tokenizer::Token::Operator(tokenizer::Operator::Assignment) = token {
                let rhs = Expression::parse(tokens, precedence)?;
                lhs = Self::BINARY(Box::new(lhs), BinaryOperator::ASSIGNMENT, Box::new(rhs));
            } else {
                let rhs = Expression::parse(tokens, precedence + 1)?;
                let op = BinaryOperator::try_from(token).unwrap();
                lhs = Expression::BINARY(Box::new(lhs), op, Box::new(rhs));
            }
        }
        Ok(lhs)
    }

    fn resolve(expression: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        match expression {
            Self::FACTOR(factor) => Ok(Self::FACTOR(Factor::resolve(factor, variables)?)),
            Self::BINARY(lhs, op, rhs) => match op {
                BinaryOperator::ASSIGNMENT if !lhs.is_identifier() => {
                    Err(String::from("Invalid lvalue in assignment"))
                }
                _ => {
                    let lhs = Box::new(Expression::resolve(*lhs, variables)?);
                    let rhs = Box::new(Expression::resolve(*rhs, variables)?);
                    Ok(Expression::BINARY(lhs, op, rhs))
                }
            },
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FACTOR(fac) => write!(f, "{fac}"),
            Self::BINARY(l, op, r) => write!(f, "{l} {op} {r}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration(String, Option<Expression>);

impl Declaration {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn initializer(self) -> Option<Expression> {
        self.1
    }

    fn resolve(declaration: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        if variables.contains_key(&declaration.0) {
            Err(String::from("Duplicate variable declaration"))
        } else {
            let id = nanoid!(21, ALPHANUMERIC);
            let name = format!("var.{}.{id}", &declaration.0);
            variables.insert(String::from(&declaration.0), String::from(&name));
            if let Some(expr) = declaration.1 {
                let initializer = Expression::resolve(expr, variables)?;
                Ok(Declaration(name, Some(initializer)))
            } else {
                Ok(Declaration(name, None))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    NULL,
    RETURN(Expression),
    EXPRESSION(Expression),
}

impl Statement {
    fn resolve(statement: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        let stmt = match statement {
            Self::EXPRESSION(expr) => Self::EXPRESSION(Expression::resolve(expr, variables)?),
            Self::RETURN(expr) => Self::RETURN(Expression::resolve(expr, variables)?),
            Self::NULL => Self::NULL,
        };
        Ok(stmt)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NULL => write!(f, ";"),
            Self::EXPRESSION(expr) => write!(f, "{};", expr),
            Self::RETURN(expr) => write!(f, "RETURN {};", expr),
        }
    }
}

// Represents a block item in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

impl BlockItem {
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                Ok(Self::Statement(Statement::NULL))
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Int)) => {
                let name = match tokens.pop_front() {
                    Some(tokenizer::Token::Identifier(id)) => Ok(id),
                    None => Err(String::from("Unexpected end of input.")),
                    _ => Err(String::from("Expected identifier after 'int'.")),
                }?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        Ok(Self::Declaration(Declaration(name, None)))
                    }
                    Some(tokenizer::Token::Operator(tokenizer::Operator::Assignment)) => {
                        let expr = Expression::parse(tokens, 0)?;
                        match tokens.pop_front() {
                            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                                Ok(Self::Declaration(Declaration(name, Some(expr))))
                            }
                            _ => Err(String::from("Expected ';' after declaration.")),
                        }
                    }
                    _ => Err(String::from("Expected ';' or '=' after variable name.")),
                }
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Return)) => {
                let expr = Expression::parse(tokens, 0)?;
                let stmt = match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        Ok(Statement::RETURN(expr))
                    }
                    _ => Err(String::from("Expected ';' after return expression.")),
                };
                stmt.map(BlockItem::Statement)
            }
            Some(token) => {
                tokens.push_front(token);
                let expr = Expression::parse(tokens, 0)?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        Ok(BlockItem::Statement(Statement::EXPRESSION(expr)))
                    }
                    _ => Err(String::from("Expected ';' after expression statement.")),
                }
            }
            None => Err(String::from("Unexpected end of input while parsing body.")),
        }
    }

    fn resolve(item: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        match item {
            Self::Declaration(decl) => {
                Ok(Self::Declaration(Declaration::resolve(decl, variables)?))
            }
            Self::Statement(stmt) => Ok(BlockItem::Statement(Statement::resolve(stmt, variables)?)),
        }
    }
}

impl fmt::Display for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declaration(decl) => write!(f, "{}", decl.0),
            Self::Statement(stmt) => write!(f, "{}", stmt),
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

    pub fn instructions(self) -> Vec<BlockItem> {
        self.1
    }

    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Int)) => {
                let name = match tokens.pop_front() {
                    Some(tokenizer::Token::Identifier(name)) => Ok(name),
                    None => Err(String::from("Unexpected end of input.")),
                    _ => Err(String::from("Expected identifier.")),
                }?;
                match (tokens.pop_front(), tokens.pop_front(), tokens.pop_front()) {
                    (
                        Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)),
                        Some(tokenizer::Token::Keyword(tokenizer::Keyword::Void)),
                        Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)),
                    ) => match tokens.pop_front() {
                        Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftBrace)) => {
                            let mut block: Vec<BlockItem> = Vec::new();
                            loop {
                                if let Some(tokenizer::Token::Delimiter(
                                    tokenizer::Delimiter::RightBrace,
                                )) = tokens.front()
                                {
                                    tokens.pop_front();
                                    break Ok(Function(name, block));
                                }
                                let item = BlockItem::parse(tokens)?;
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

    fn resolve(function: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        let name = String::from(&function.0);
        let mut items: Vec<BlockItem> = Vec::new();
        for item in function.1 {
            items.push(BlockItem::resolve(item, variables)?);
        }
        Ok(Function(name, items))
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

#[derive(Debug, Clone, PartialEq)]
pub struct Program(Function);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn parse(tokens: Vec<tokenizer::Token>) -> Result<Program, String> {
    let mut tokens: VecDeque<tokenizer::Token> = VecDeque::from(tokens);

    let main = Function::parse(&mut tokens)?;
    let program = Program(main);

    if tokens.is_empty() {
        Ok(program)
    } else {
        Err(String::from("Unexpected token after program end."))
    }
}

pub fn validate(ast: Program) -> Result<Program, String> {
    let main = Function::from(ast);
    let mut vars: HashMap<String, String> = HashMap::new();
    Ok(Program(Function::resolve(main, &mut vars)?))
}
