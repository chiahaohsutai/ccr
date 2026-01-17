use core::fmt;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;

use super::tokenizer;

/// Represents unary operators that operate on a single operand.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Decrement,
    Increment,
    Negation,
    LogicalNot,
    Complement,
}

impl UnaryOperator {
    /// Returns `true` if this operator is the increment or decrement operator.
    fn is_incr_or_decr(&self) -> bool {
        matches!(self, Self::Increment | Self::Decrement)
    }
}

impl TryFrom<tokenizer::Operator> for UnaryOperator {
    type Error = String;

    fn try_from(value: tokenizer::Operator) -> Result<Self, Self::Error> {
        match value {
            tokenizer::Operator::Decrement => Ok(Self::Decrement),
            tokenizer::Operator::Increment => Ok(Self::Increment),
            tokenizer::Operator::Negation => Ok(Self::Negation),
            tokenizer::Operator::Complement => Ok(Self::Complement),
            tokenizer::Operator::LogicalNot => Ok(Self::LogicalNot),
            _ => Err(format!("Operator '{value}' is not a unary operator.")),
        }
    }
}

impl TryFrom<tokenizer::Token> for UnaryOperator {
    type Error = String;

    fn try_from(value: tokenizer::Token) -> Result<Self, Self::Error> {
        match value {
            tokenizer::Token::Operator(op) => Self::try_from(op),
            _ => Err(format!("Token '{value}' is not a unary operator.")),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Decrement => write!(f, "--"),
            Self::Increment => write!(f, "++"),
            Self::Complement => write!(f, "~"),
            Self::LogicalNot => write!(f, "!"),
            Self::Negation => write!(f, "-"),
        }
    }
}

/// Represents binary operators that operate on two operands.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Divide,
    Substract,
    Multiply,
    Remainder,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LeftShift,
    RightShift,
    LogicalAnd,
    LogicalOr,
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
    Assignment,
    AddAssignment,
    SubAssignment,
    DivAssignment,
    ProdAssignment,
    RemAssignment,
    AndAssignment,
    OrAssignment,
    XorAssignment,
    LShiftAssignment,
    RShiftAssignment,
}

impl BinaryOperator {
    // Returns `true` if this operator is an assignment or compound assignment operator.
    pub fn is_assignment(&self) -> bool {
        match self {
            Self::AddAssignment => true,
            Self::SubAssignment => true,
            Self::DivAssignment => true,
            Self::RemAssignment => true,
            Self::ProdAssignment => true,
            Self::AndAssignment => true,
            Self::OrAssignment => true,
            Self::XorAssignment => true,
            Self::LShiftAssignment => true,
            Self::RShiftAssignment => true,
            Self::Assignment => true,
            _ => false,
        }
    }
}

impl TryFrom<tokenizer::Operator> for BinaryOperator {
    type Error = String;

    fn try_from(value: tokenizer::Operator) -> Result<Self, Self::Error> {
        match value {
            tokenizer::Operator::Addition => Ok(Self::Add),
            tokenizer::Operator::Negation => Ok(Self::Substract),
            tokenizer::Operator::Product => Ok(Self::Multiply),
            tokenizer::Operator::Division => Ok(Self::Divide),
            tokenizer::Operator::Remainder => Ok(Self::Remainder),
            tokenizer::Operator::BitwiseAnd => Ok(Self::BitwiseAnd),
            tokenizer::Operator::BitwiseOr => Ok(Self::BitwiseOr),
            tokenizer::Operator::BitwiseXor => Ok(Self::BitwiseXor),
            tokenizer::Operator::LeftShift => Ok(Self::LeftShift),
            tokenizer::Operator::RightShift => Ok(Self::RightShift),
            tokenizer::Operator::LogicalAnd => Ok(Self::LogicalAnd),
            tokenizer::Operator::LogicalOr => Ok(Self::LogicalOr),
            tokenizer::Operator::EqualEqual => Ok(Self::EqualEqual),
            tokenizer::Operator::NotEqual => Ok(Self::NotEqual),
            tokenizer::Operator::LessThan => Ok(Self::LessThan),
            tokenizer::Operator::GreaterThan => Ok(Self::GreaterThan),
            tokenizer::Operator::LessThanOrEq => Ok(Self::LessThanOrEq),
            tokenizer::Operator::GreaterThanOrEq => Ok(Self::GreaterThanOrEq),
            tokenizer::Operator::Assignment => Ok(Self::Assignment),
            tokenizer::Operator::AddAssignment => Ok(Self::AddAssignment),
            tokenizer::Operator::SubAssignment => Ok(Self::SubAssignment),
            tokenizer::Operator::DivAssignment => Ok(Self::DivAssignment),
            tokenizer::Operator::RemAssignment => Ok(Self::RemAssignment),
            tokenizer::Operator::ProdAssignment => Ok(Self::ProdAssignment),
            tokenizer::Operator::AndAssignment => Ok(Self::AndAssignment),
            tokenizer::Operator::OrAssignment => Ok(Self::OrAssignment),
            tokenizer::Operator::XorAssignment => Ok(Self::XorAssignment),
            tokenizer::Operator::LShiftAssignment => Ok(Self::LShiftAssignment),
            tokenizer::Operator::RShiftAssignment => Ok(Self::RShiftAssignment),
            _ => Err(format!("Operator '{value}' is not a binary operator.")),
        }
    }
}

impl TryFrom<tokenizer::Token> for BinaryOperator {
    type Error = String;

    fn try_from(token: tokenizer::Token) -> Result<Self, Self::Error> {
        match token {
            tokenizer::Token::Operator(operator) => Self::try_from(operator),
            token => Err(format!("Token {token} is not an operator.")),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Substract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Remainder => write!(f, "%"),
            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseXor => write!(f, "^"),
            Self::LeftShift => write!(f, "<<"),
            Self::RightShift => write!(f, ">>"),
            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalOr => write!(f, "||"),
            Self::EqualEqual => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanOrEq => write!(f, "<="),
            Self::GreaterThanOrEq => write!(f, ">="),
            Self::Assignment => write!(f, "="),
            Self::AddAssignment => write!(f, "+="),
            Self::SubAssignment => write!(f, "-="),
            Self::RemAssignment => write!(f, "%="),
            Self::DivAssignment => write!(f, "/="),
            Self::ProdAssignment => write!(f, "*="),
            Self::AndAssignment => write!(f, "&="),
            Self::OrAssignment => write!(f, "|="),
            Self::XorAssignment => write!(f, "^="),
            Self::LShiftAssignment => write!(f, "<<="),
            Self::RShiftAssignment => write!(f, ">>="),
        }
    }
}

/// Represents the fixity of an operator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Fixity {
    Prefix,
    Postfix,
}

/// Represents a factor in an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Factor {
    Int(u64),
    Identifier(String),
    Unary(UnaryOperator, Fixity, Box<Factor>),
    Expression(Box<Expression>),
}

impl Factor {
    /// Returns `true` if this factor is an identifier.
    fn is_ident(&self) -> bool {
        match self {
            Self::Identifier(_) => true,
            Self::Expression(expression) => expression.is_ident(),
            _ => false,
        }
    }

    /// Parses a factor from the front of the token stream.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        let mut factor = match tokens.pop_front() {
            Some(tokenizer::Token::Identifier(name)) => Ok(Self::Identifier(name)),
            Some(tokenizer::Token::Constant(constant)) => Ok(Self::Int(constant)),
            Some(tokenizer::Token::Operator(operator)) => {
                let operator = UnaryOperator::try_from(operator)?;
                let factor = Box::new(Self::parse(tokens)?);
                Ok(Factor::Unary(operator, Fixity::Prefix, factor))
            }
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                let expression = Expression::parse(tokens, 0)?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                        Ok(Self::Expression(Box::new(expression)))
                    }
                    _ => Err(String::from("Expected ')' after expression.")),
                }
            }
            Some(token) => Err(format!("Malformed factor, found {token}.")),
            None => Err(String::from("Unexpected end of input while parsing factor")),
        }?;

        while tokens.front().is_some_and(|t| t.is_incr_or_decr_op()) {
            let token = tokens.pop_front().unwrap();
            let operand = Box::new(factor);
            let operator = UnaryOperator::try_from(token)?;
            if let UnaryOperator::Increment = operator {
                factor = Factor::Unary(UnaryOperator::Increment, Fixity::Postfix, operand);
            } else {
                factor = Factor::Unary(UnaryOperator::Decrement, Fixity::Postfix, operand);
            }
        }

        Ok(factor)
    }

    /// Resolves identifiers within the factor using the provided symbol table.
    fn resolve(factor: Self, env: &mut Environment) -> Result<Self, String> {
        match factor {
            Self::Identifier(identifier) => match env.variables.find(&identifier) {
                Some(ident) => Ok(Self::Identifier(String::from(ident))),
                None => Err(String::from("Undeclared variable")),
            },
            Self::Expression(expression) => {
                let expression = Box::new(Expression::resolve(*expression, env)?);
                Ok(Self::Expression(expression))
            }
            Self::Unary(operator, fixity, factor) => {
                let factor = Factor::resolve(*factor, env)?;
                if operator.is_incr_or_decr() && !factor.is_ident() {
                    Err(String::from("Invalid operand in unary operation"))
                } else {
                    Ok(Self::Unary(operator, fixity, Box::new(factor)))
                }
            }
            Self::Int(_) => Ok(factor),
        }
    }
}

impl fmt::Display for Factor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(number) => write!(f, "{number}"),
            Self::Unary(operator, fixity, factor) => match fixity {
                Fixity::Prefix => write!(f, "{operator}{factor}"),
                Fixity::Postfix => write!(f, "{factor}{operator}"),
            },
            Self::Expression(expression) => write!(f, "({expression})"),
            Self::Identifier(identifier) => write!(f, "{identifier}"),
        }
    }
}

/// Represents an expression node in the abstract syntax tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Factor(Factor),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
}

impl Expression {
    /// Returns `true` if this expression resolves to an identifier.
    fn is_ident(&self) -> bool {
        match self {
            Self::Factor(factor) => factor.is_ident(),
            _ => false,
        }
    }

    /// Parses an expression from the token stream using precedence climbing.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>, precedence: u64) -> Result<Self, String> {
        let mut lhs = Expression::Factor(Factor::parse(tokens)?);

        while tokens
            .front()
            .is_some_and(|t| (t.is_binary_op() || t.is_eroteme()) && t.precedence() >= precedence)
        {
            let token = tokens.pop_front().unwrap();
            let precedence = token.precedence();

            match token {
                tokenizer::Token::Operator(operator) if token.is_assignment_op() => {
                    let rhs = Expression::parse(tokens, precedence)?;
                    let operator = BinaryOperator::try_from(operator)?;
                    lhs = Self::Binary(Box::new(lhs), operator, Box::new(rhs));
                }
                tokenizer::Token::Delimiter(tokenizer::Delimiter::QuestionMark) => {
                    let then = Box::new(Self::parse(tokens, 0)?);
                    let next = tokens.pop_front();
                    if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Colon)) = next {
                        let otherwise = Box::new(Expression::parse(tokens, precedence)?);
                        lhs = Expression::Conditional(Box::new(lhs), then, otherwise)
                    } else {
                        Err(format!("Expected ':' found: {next:?}"))?;
                    }
                }
                _ => {
                    let rhs = Expression::parse(tokens, precedence + 1)?;
                    let operator = BinaryOperator::try_from(token)?;
                    lhs = Expression::Binary(Box::new(lhs), operator, Box::new(rhs));
                }
            }
        }
        Ok(lhs)
    }

    /// Resolves identifiers within the expression using the provided symbol table.
    /// # Errors
    /// Returns an error if an assignment has an invalid left-hand side or if
    /// an identifier cannot be resolved.
    fn resolve(expression: Self, env: &mut Environment) -> Result<Self, String> {
        match expression {
            Self::Factor(factor) => Ok(Self::Factor(Factor::resolve(factor, env)?)),
            Self::Binary(lhs, operator, rhs) => {
                if operator.is_assignment() && !lhs.is_ident() {
                    Err(format!("Invalid lvalue in assignment: {lhs}"))
                } else {
                    let lhs = Box::new(Expression::resolve(*lhs, env)?);
                    let rhs = Box::new(Expression::resolve(*rhs, env)?);
                    Ok(Expression::Binary(lhs, operator, rhs))
                }
            }
            Self::Conditional(cond, then, otherwise) => {
                let cond = Box::new(Self::resolve(*cond, env)?);
                let then = Box::new(Self::resolve(*then, env)?);
                Ok(Self::Conditional(
                    cond,
                    then,
                    Box::new(Self::resolve(*otherwise, env)?),
                ))
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Factor(fac) => write!(f, "{fac}"),
            Self::Binary(lhs, op, rhs) => write!(f, "{lhs} {op} {rhs}"),
            Self::Conditional(cond, then, otherwise) => write!(f, "{cond} ? {then} : {otherwise}"),
        }
    }
}

/// Represents a variable declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Declaration(String, Option<Expression>);

impl Declaration {
    /// Returns the declared variable name.
    pub fn name(&self) -> &str {
        &self.0
    }

    /// Returns the initializer expression, if one is present.
    pub fn initializer(self) -> Option<Expression> {
        self.1
    }

    /// Parses a declaration
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Int)) => {
                let name = match tokens.pop_front() {
                    Some(tokenizer::Token::Identifier(identifier)) => Ok(identifier),
                    None => Err(String::from("Unexpected end of input.")),
                    _ => Err(String::from("Expected identifier after 'int'.")),
                }?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        Ok(Self(name, None))
                    }
                    Some(tokenizer::Token::Operator(tokenizer::Operator::Assignment)) => {
                        let expr = Expression::parse(tokens, 0)?;
                        match tokens.pop_front() {
                            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                                Ok(Self(name, Some(expr)))
                            }
                            tok => Err(format!("Expected ';' after declaration, found: {tok:?}")),
                        }
                    }
                    _ => Err(String::from("Expected ';' or '=' after variable name.")),
                }
            }
            tok => Err(format!("Expected 'int' found: {tok:?}")),
        }
    }

    /// Resolves a variable declaration within the given symbol table.
    fn resolve(declaration: Self, env: &mut Environment) -> Result<Self, String> {
        let id = nanoid!(21, ALPHANUMERIC);
        let name = format!("var.{}.{id}", &declaration.0);

        if let None = env
            .variables
            .insert(String::from(&declaration.0), String::from(&name))
        {
            if let Some(expr) = declaration.1 {
                let initializer = Expression::resolve(expr, env)?;
                Ok(Declaration(name, Some(initializer)))
            } else {
                Ok(Declaration(name, None))
            }
        } else {
            Err(String::from("Duplicate variable declaration"))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInit {
    InitDecl(Declaration),
    InitExpr(Expression),
}

impl ForInit {
    // Parses the for loop initializer header
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.front() {
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Int)) => {
                Ok(Self::InitDecl(Declaration::parse(tokens)?))
            }
            _ => {
                let init = Self::InitExpr(Expression::parse(tokens, 0)?);
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => Ok(init),
                    tok => Err(format!("Expected ';' found: {tok:?}")),
                }
            }
        }
    }

    fn resolve(init: Self, env: &mut Environment) -> Result<Self, String> {
        match init {
            Self::InitDecl(decl) => Ok(Self::InitDecl(Declaration::resolve(decl, env)?)),
            Self::InitExpr(expr) => Ok(Self::InitExpr(Expression::resolve(expr, env)?)),
        }
    }
}

/// Represents the body of a switch case.
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchCaseBody {
    Block(Block),
    Body(Vec<BlockItem>),
}

fn is_switch_case_end(token: &tokenizer::Token) -> bool {
    matches!(
        token,
        tokenizer::Token::Keyword(tokenizer::Keyword::Case)
            | tokenizer::Token::Keyword(tokenizer::Keyword::Default)
            | tokenizer::Token::Delimiter(tokenizer::Delimiter::RightBrace)
    )
}

impl SwitchCaseBody {
    /// Parses a switch case body.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftBrace)) = tokens.front() {
            let body = Block::parse(tokens)?;
            Ok(Self::Block(body))
        } else {
            let mut items: Vec<BlockItem> = Vec::new();
            while tokens.front().is_some_and(|t| !is_switch_case_end(t)) {
                let item = BlockItem::parse(tokens)?;
                items.push(item);
            }
            Ok(Self::Body(items))
        }
    }
}

impl fmt::Display for SwitchCaseBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Block(block) => write!(f, "{block}"),
            Self::Body(items) => {
                let items: Vec<String> = items.iter().map(|i| i.to_string()).collect();
                write!(f, "{}", items.join("\n"))
            }
        }
    }
}

/// Represents a switch statment case.
#[derive(Debug, Clone, PartialEq)]
pub enum SwitchCase {
    Default(Option<SwitchCaseBody>),
    Case(Expression, Option<SwitchCaseBody>),
}

impl SwitchCase {
    /// Parses a switch statament case
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Case)) => {
                let expr = Expression::parse(tokens, 0)?;
                let next = tokens.pop_front();
                if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Colon)) = next {
                    let body = SwitchCaseBody::parse(tokens)?;
                    match body {
                        SwitchCaseBody::Body(b) if b.len() == 0 => Ok(Self::Case(expr, None)),
                        _ => Ok(Self::Case(expr, Some(body))),
                    }
                } else {
                    Err(format!("Expected ':' found: {next:?}, {tokens:?}"))
                }
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Default)) => {
                let next = tokens.pop_front();
                if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Colon)) = next {
                    let body = SwitchCaseBody::parse(tokens)?;
                    match body {
                        SwitchCaseBody::Body(b) if b.len() == 0 => Ok(Self::Default(None)),
                        _ => Ok(Self::Default(Some(body))),
                    }
                } else {
                    Err(format!("Expected ':' found: {next:?}, {tokens:?}"))
                }
            }
            tok => Err(format!(
                "Expected 'case' or 'default' found: {tok:?}, {tokens:?}"
            )),
        }
    }
}

impl fmt::Display for SwitchCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Default(body) => write!(f, "default:\n{body:?}"),
            Self::Case(case, body) => write!(f, "case {case}:\n{body:?}"),
        }
    }
}

/// Represents a statement in the abstract syntax tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Null,
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Goto(String),
    Label(String, Box<Statement>),
    Compound(Block),
    Break(String),
    Continue(String),
    While(Expression, Box<Statement>, String),
    DoWhile(Box<Statement>, Expression, String),
    Switch(Expression, Vec<SwitchCase>),
    For(
        Option<ForInit>,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
        String,
    ),
}

impl Statement {
    fn parse_if(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                let expression = Expression::parse(tokens, 0)?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                        let then = Box::new(Statement::parse(tokens)?);
                        if let Some(tokenizer::Token::Keyword(tokenizer::Keyword::Else)) =
                            tokens.front()
                        {
                            let _ = tokens.pop_front();
                            let otherwise = Box::new(Statement::parse(tokens)?);
                            Ok(Self::If(expression, then, Some(otherwise)))
                        } else {
                            Ok(Self::If(expression, then, None))
                        }
                    }
                    tok => Err(format!("Expected '(' found {tok:?} instead.")),
                }
            }
            tok => Err(format!("Expected '(' found {tok:?} instead.")),
        }
    }

    fn parse_return(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        let expression = Expression::parse(tokens, 0)?;
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                Ok(Statement::Return(expression))
            }
            _ => Err(String::from("Expected ';' after return expression.")),
        }
    }

    fn parse_goto(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Identifier(ident)) => match tokens.pop_front() {
                Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                    Ok(Self::Goto(ident))
                }
                tok => Err(format!("Expected ';' after statament found: {tok:?}")),
            },
            tok => Err(format!("Expected label found: {tok:?}")),
        }
    }

    fn parse_break(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                Ok(Self::Break(String::new()))
            }
            tok => Err(format!("Expected ';' found: {tok:?}")),
        }
    }

    fn parse_continue(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                Ok(Self::Continue(String::new()))
            }
            tok => Err(format!("Expected ';' found: {tok:?}")),
        }
    }

    fn parse_while(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                let cond = Expression::parse(tokens, 0)?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                        let body = Self::parse(tokens)?;
                        let label = format!("loop.{}", nanoid!(21, ALPHANUMERIC));
                        Ok(Self::While(cond, Box::new(body), label))
                    }
                    tok => Err(format!("Expected ')' found {tok:?}")),
                }
            }
            tok => Err(format!("Expected '(' found {tok:?}")),
        }
    }

    fn parse_dowhile(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        let body = Self::parse(tokens)?;
        match tokens.pop_front() {
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::While)) => {
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                        let cond = Expression::parse(tokens, 0)?;
                        match tokens.pop_front() {
                            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                                match tokens.pop_front() {
                                    Some(tokenizer::Token::Delimiter(
                                        tokenizer::Delimiter::Semicolon,
                                    )) => {
                                        let label = format!("loop.{}", nanoid!(21, ALPHANUMERIC));
                                        Ok(Self::DoWhile(Box::new(body), cond, label))
                                    }
                                    tok => Err(format!("Expected ';' found: {tok:?}")),
                                }
                            }
                            tok => Err(format!("Expected ')' found {tok:?}")),
                        }
                    }
                    tok => Err(format!("Expected '(' found {tok:?}")),
                }
            }
            tok => Err(format!("Expected 'while' found: {tok:?}")),
        }
    }

    fn parse_for(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                let init = match tokens.front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        let _ = tokens.pop_front();
                        None
                    }
                    _ => Some(ForInit::parse(tokens)?),
                };
                let cond = match tokens.front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        let _ = tokens.pop_front();
                        None
                    }
                    _ => {
                        let cond = Expression::parse(tokens, 0)?;
                        match tokens.pop_front() {
                            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                                Some(cond)
                            }
                            tok => Err(format!("Expected ';' found: {tok:?}, {tokens:?}"))?,
                        }
                    }
                };
                let post = match tokens.front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                        let _ = tokens.pop_front();
                        None
                    }
                    _ => {
                        let post = Expression::parse(tokens, 0)?;
                        match tokens.pop_front() {
                            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) => {
                                Some(post)
                            }
                            tok => Err(format!("Expected ')' found: {tok:?}, {tokens:?}"))?,
                        }
                    }
                };
                let body = Box::new(Self::parse(tokens)?);
                let label = format!("loop.{}", nanoid!(21, ALPHANUMERIC));
                Ok(Self::For(init, cond, post, body, label))
            }
            tok => Err(format!("Expected '(' found: {tok:?}, {tokens:?}")),
        }
    }

    fn parse_switch(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftParen)) => {
                let expr = Expression::parse(tokens, 0)?;
                let next = tokens.pop_front();
                if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightParen)) = next {
                    let next = tokens.pop_front();
                    if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftBrace)) = next
                    {
                        let mut cases: Vec<SwitchCase> = Vec::new();
                        loop {
                            if let Some(tokenizer::Token::Delimiter(
                                tokenizer::Delimiter::RightBrace,
                            )) = tokens.front()
                            {
                                let _ = tokens.pop_front();
                                break Ok(Self::Switch(expr, cases));
                            }
                            let case = SwitchCase::parse(tokens)?;
                            cases.push(case);
                        }
                    } else {
                        Err(format!("Expected '{{' found: {next:?}, {tokens:?}"))
                    }
                } else {
                    Err(format!("Expected ')' found: {next:?}, {tokens:?}"))
                }
            }
            tok => Err(format!("Expected '(' found: {tok:?}, {tokens:?}")),
        }
    }

    /// Parses a statement from the token stream.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => Ok(Self::Null),
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::If)) => Self::parse_if(tokens),
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Return)) => {
                Self::parse_return(tokens)
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Goto)) => Self::parse_goto(tokens),
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Break)) => Self::parse_break(tokens),
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::While)) => Self::parse_while(tokens),
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Do)) => Self::parse_dowhile(tokens),
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Continue)) => {
                Self::parse_continue(tokens)
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::For)) => Self::parse_for(tokens),
            Some(tokenizer::Token::Identifier(i))
                if tokens.front().is_some_and(|t| t.is_colon()) =>
            {
                let _ = tokens.pop_front();
                let stmt = Self::parse(tokens)?;
                Ok(Self::Label(i, Box::new(stmt)))
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Switch)) => {
                Self::parse_switch(tokens)
            }
            Some(token) => {
                if let tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftBrace) = token {
                    tokens.push_front(token);
                    Ok(Self::Compound(Block::parse(tokens)?))
                } else {
                    tokens.push_front(token);
                    let expression = Expression::parse(tokens, 0)?;
                    match tokens.pop_front() {
                        Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                            Ok(Statement::Expression(expression))
                        }
                        _ => Err(String::from("Expected ';' after expression statement.")),
                    }
                }
            }
            None => Err(String::from("Unexpected end of input while parsing stmt.")),
        }
    }

    /// Resolves identifiers within the statement using the provided symbol table.
    fn resolve(statement: Self, env: &mut Environment) -> Result<Self, String> {
        match statement {
            Self::Expression(expression) => {
                let expression = Expression::resolve(expression, env)?;
                Ok(Self::Expression(expression))
            }
            Self::Return(expression) => Ok(Self::Return(Expression::resolve(expression, env)?)),
            Self::Null => Ok(Self::Null),
            Self::If(cond, then, otherwise) => {
                let cond = Expression::resolve(cond, env)?;
                let then = Box::new(Self::resolve(*then, env)?);
                if let Some(otherwise) = otherwise {
                    let otherwise = Box::new(Self::resolve(*otherwise, env)?);
                    Ok(Self::If(cond, then, Some(otherwise)))
                } else {
                    Ok(Self::If(cond, then, None))
                }
            }
            Self::Goto(label) => Ok(Self::Goto(label)),
            Self::Label(label, stmt) => {
                if env.labels.insert(label.clone()) {
                    let stmt = Self::resolve(*stmt, env)?;
                    Ok(Self::Label(label, Box::new(stmt)))
                } else {
                    Err(format!("Duplicate goto lable found: {label}"))
                }
            }
            Self::Compound(block) => Ok(Self::Compound(Block::resolve(block, env)?)),
            Self::While(cond, body, label) => {
                env.loops.push_back(label.clone());
                let cond = Expression::resolve(cond, env)?;
                let body = Statement::resolve(*body, env)?;
                let _ = env.loops.pop_back();
                Ok(Self::While(cond, Box::new(body), label))
            }
            Self::DoWhile(body, cond, label) => {
                env.loops.push_back(label.clone());
                let body = Self::resolve(*body, env)?;
                let cond = Expression::resolve(cond, env)?;
                let _ = env.loops.pop_back();
                Ok(Self::DoWhile(Box::new(body), cond, label))
            }
            Self::For(init, cond, post, body, label) => {
                env.variables.enter();
                env.loops.push_back(label.clone());
                let init = match init {
                    Some(init) => Some(ForInit::resolve(init, env)?),
                    None => None,
                };
                let cond = match cond {
                    Some(cond) => Some(Expression::resolve(cond, env)?),
                    None => None,
                };
                let post = match post {
                    Some(post) => Some(Expression::resolve(post, env)?),
                    None => None,
                };
                let body = Self::resolve(*body, env)?;
                env.variables.exit();
                let _ = env.loops.pop_back();
                Ok(Self::For(init, cond, post, Box::new(body), label))
            }
            Self::Break(_) => match env.loops.back() {
                Some(label) => Ok(Self::Break(String::from(label))),
                _ => Err(String::from("Break statement outside loop.")),
            },
            Self::Continue(_) => match env.loops.back() {
                Some(label) => Ok(Self::Continue(String::from(label))),
                _ => Err(String::from("Continue statement outside loop")),
            },
            _ => todo!(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, ";"),
            Self::Expression(expression) => write!(f, "{expression};"),
            Self::Return(expression) => write!(f, "return {expression};"),
            Self::If(cond, then, otherwise) => match otherwise {
                Some(otherwise) => write!(f, "if {cond} then {then} else {otherwise}"),
                None => write!(f, "if {cond} then {then}"),
            },
            Self::Goto(label) => write!(f, "goto {label}"),
            Self::Label(label, stmt) => write!(f, "{label}: {stmt}"),
            Self::Compound(block) => write!(f, "{block}"),
            Self::Break(_) => write!(f, "break"),
            Self::Continue(_) => write!(f, "continue"),
            Self::While(cond, stmt, _) => write!(f, "while {cond}\n{stmt}"),
            Self::DoWhile(stmt, cond, _) => write!(f, "do\n{stmt}\nwhile {cond}"),
            Self::For(init, cond, post, stmt, _) => {
                write!(f, "for {init:?} | {cond:?} | {post:?}\n{stmt}")
            }
            Self::Switch(value, cases) => {
                let cases: Vec<String> = cases.iter().map(|c| c.to_string()).collect();
                write!(f, "switch ({value}):\n{}", cases.join("\n"))
            }
        }
    }
}

/// Represents an item within a block.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

impl BlockItem {
    /// Parses a single block item from the token stream.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.front() {
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Int)) => {
                Ok(Self::Declaration(Declaration::parse(tokens)?))
            }
            Some(_) => Ok(BlockItem::Statement(Statement::parse(tokens)?)),
            None => Err(String::from("Unexpected end of input while parsing body.")),
        }
    }

    /// Resolves identifiers within the block item using the provided symbol table.
    fn resolve(item: Self, env: &mut Environment) -> Result<Self, String> {
        match item {
            Self::Declaration(declaration) => {
                let declaration = Declaration::resolve(declaration, env)?;
                Ok(Self::Declaration(declaration))
            }
            Self::Statement(stmt) => {
                let statement = Statement::resolve(stmt, env)?;
                Ok(BlockItem::Statement(statement))
            }
        }
    }
}

impl fmt::Display for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Declaration(declaration) => write!(f, "{} {:?}", declaration.0, declaration.1),
            Self::Statement(statement) => write!(f, "{}", statement),
        }
    }
}

/// Represents a scoped sequence of statements or expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct Block(Vec<BlockItem>);

impl Block {
    /// Consumes the block and returns its items.
    pub fn items(self) -> Vec<BlockItem> {
        self.0
    }

    /// Parses a sequence block item from the token stream.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftBrace)) => {
                let mut block: Vec<BlockItem> = Vec::new();
                loop {
                    if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightBrace)) =
                        tokens.front()
                    {
                        tokens.pop_front();
                        break Ok(Self(block));
                    }
                    let item = BlockItem::parse(tokens)?;
                    block.push(item);
                }
            }
            _ => Err(String::from("Expected '{' at the start of fn body.")),
        }
    }

    fn resolve(block: Self, env: &mut Environment) -> Result<Self, String> {
        let mut items: Vec<BlockItem> = Vec::new();
        env.variables.enter();
        for item in block.0 {
            items.push(BlockItem::resolve(item, env)?);
        }
        env.variables.exit();
        Ok(Block(items))
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items = self
            .0
            .iter()
            .map(|i| format!("{}", i))
            .collect::<Vec<String>>();
        write!(f, "{}", items.join("\n"))
    }
}

/// Represents a function definition in the abstract syntax tree.
#[derive(Debug, Clone, PartialEq)]
pub struct Function(String, Block);

impl Function {
    /// Returns the function name.
    pub fn name(&self) -> &str {
        &self.0
    }

    /// Returns the sequence of block items that make up the function body.
    pub fn instructions(self) -> Vec<BlockItem> {
        self.1.0
    }

    /// Parses a function definition from the token stream.
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
                    ) => Ok(Function(name, Block::parse(tokens)?)),
                    _ => Err(String::from("Expected '(void)' after fn name.")),
                }
            }
            None => Err(String::from("Unexpected end of input while parsing fn.")),
            _ => Err(String::from("Expected 'int' keyword at the start of a fn.")),
        }
    }

    /// Resolves identifiers within the function body using the provided symbol table.
    fn resolve(function: Self, env: &mut Environment) -> Result<Self, String> {
        let name = String::from(&function.0);
        let block = Block::resolve(function.1, env)?;
        for item in block.0.iter() {
            if let BlockItem::Statement(Statement::Goto(label)) = item {
                if env.labels.get(label).is_none() {
                    return Err(format!("Goto label not defined: {label}"));
                }
            }
        }
        Ok(Function(name, block))
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FN {}\n{}\nEND FN {}", self.0, self.1, self.0)
    }
}

impl From<Program> for Function {
    fn from(program: Program) -> Self {
        program.0
    }
}

/// Represents a complete program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program(Function);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Parses a complete program from a sequence of tokens.
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

struct VarMap<K, V>(Vec<HashMap<K, V>>);

impl<K, V> VarMap<K, V>
where
    K: Eq + Hash,
{
    /// Creates a new environment initialized with a single global scope.
    fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    /// Looks up `k` from the innermost scope outward, returning the first match.
    fn find<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        for env in self.0.iter().rev() {
            if let Some(v) = env.get(k) {
                return Some(v);
            }
        }
        None
    }

    /// Inserts `k -> v` into the current  scope and returns the old value if it existed.
    fn insert(&mut self, k: K, v: V) -> Option<V>
    where
        K: Eq + Hash,
    {
        self.0.last_mut().map(|map| map.insert(k, v)).flatten()
    }

    /// Enters a new (nested) scope by pushing an empty scope frame onto the environment.
    fn enter(&mut self) {
        self.0.push(HashMap::new());
    }

    /// Pops (exits) the current scope, discarding all bindings declared in it.
    fn exit(&mut self) {
        let _ = self.0.pop();
    }
}

struct LabelMap<K>(HashSet<K>);

impl<K> LabelMap<K>
where
    K: Eq + Hash,
{
    /// Creates an empty label set.
    fn new() -> Self {
        Self(HashSet::new())
    }

    /// Returns a reference to the stored label equal to `k`, if present.
    fn get<Q: ?Sized>(&self, k: &Q) -> Option<&K>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(k)
    }

    /// Inserts `k` into the label set and returns `true` if it was newly added.
    fn insert(&mut self, k: K) -> bool
    where
        K: Eq + Hash,
    {
        self.0.insert(k)
    }
}

struct Environment {
    variables: VarMap<String, String>,
    labels: LabelMap<String>,
    loops: VecDeque<String>,
}

impl Environment {
    /// Creates a new environment with empty variable scopes and an empty label set.
    fn new() -> Self {
        Self {
            variables: VarMap::new(),
            labels: LabelMap::new(),
            loops: VecDeque::new(),
        }
    }
}

/// Validates and resolves the program's abstract syntax tree.
pub fn validate(ast: Program) -> Result<Program, String> {
    let main = Function::from(ast);
    let mut variables: Environment = Environment::new();
    Ok(Program(Function::resolve(main, &mut variables)?))
}
