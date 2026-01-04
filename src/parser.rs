use core::fmt;
use std::collections::{HashMap, VecDeque};

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
            _ => Err(String::from("Token is not an operator.")),
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
///
/// Fixity describes whether an operator appears in prefix or postfix position
/// relative to its operand.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Fixity {
    Prefix,
    Postfix,
}

/// Represents a factor in an expression.
///
/// A factor is the smallest unit in an expression and may be a constant,
/// identifier, unary expression, or a parenthesized subexpression.
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
    ///
    /// A factor may be a constant, identifier, unary expression, or a
    /// parenthesized expression. Consumes the tokens required to form
    /// the factor and returns an error if the input is malformed.
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
    ///
    /// Replaces identifiers with their resolved values, recursively resolving
    /// nested expressions and unary factors. Returns an error if an identifier
    /// is undeclared.
    fn resolve(factor: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        match factor {
            Self::Identifier(identifier) => match variables.get(&identifier) {
                Some(ident) => Ok(Self::Identifier(String::from(ident))),
                None => Err(String::from("Undeclared variable")),
            },
            Self::Expression(expression) => {
                let expression = Box::new(Expression::resolve(*expression, variables)?);
                Ok(Self::Expression(expression))
            }
            Self::Unary(operator, fixity, factor) => {
                let factor = Factor::resolve(*factor, variables)?;
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
///
/// An expression may be a single factor or a binary operation composed
/// of two subexpressions and a binary operator.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Factor(Factor),
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
    ///
    /// Constructs an expression tree by repeatedly parsing binary operators
    /// according to their precedence. Tokens are consumed from the front of
    /// the stream. Returns an error if the expression is malformed.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>, precedence: u64) -> Result<Self, String> {
        let mut lhs = Expression::Factor(Factor::parse(tokens)?);

        while tokens
            .front()
            .is_some_and(|t| t.is_binary_op() && t.precedence() >= precedence)
        {
            let token = tokens.pop_front().unwrap();
            let precedence = token.precedence();

            match token {
                tokenizer::Token::Operator(operator) if token.is_assignment_op() => {
                    let rhs = Expression::parse(tokens, precedence)?;
                    let operator = BinaryOperator::try_from(operator)?;
                    lhs = Self::Binary(Box::new(lhs), operator, Box::new(rhs));
                }
                _ => {
                    let rhs = Expression::parse(tokens, precedence + 1)?;
                    let operator = BinaryOperator::try_from(token).unwrap();
                    lhs = Expression::Binary(Box::new(lhs), operator, Box::new(rhs));
                }
            }
        }
        Ok(lhs)
    }

    /// Resolves identifiers within the expression using the provided symbol table.
    ///
    /// Recursively resolves all subexpressions and validates assignment targets.
    ///
    /// # Errors
    /// Returns an error if an assignment has an invalid left-hand side or if
    /// an identifier cannot be resolved.
    fn resolve(expression: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        match expression {
            Self::Factor(factor) => Ok(Self::Factor(Factor::resolve(factor, variables)?)),
            Self::Binary(lhs, operator, rhs) => {
                if operator.is_assignment() && !lhs.is_ident() {
                    Err(format!("Invalid lvalue in assignment: {lhs}"))
                } else {
                    let lhs = Box::new(Expression::resolve(*lhs, variables)?);
                    let rhs = Box::new(Expression::resolve(*rhs, variables)?);
                    Ok(Expression::Binary(lhs, operator, rhs))
                }
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Factor(fac) => write!(f, "{fac}"),
            Self::Binary(l, op, r) => write!(f, "{l} {op} {r}"),
        }
    }
}

/// Represents a variable declaration.
///
/// A declaration consists of an identifier and an optional initializer
/// expression.
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

    /// Resolves a variable declaration within the given symbol table.
    ///
    /// Assigns a unique internal name to the declared variable, inserts it
    /// into the symbol table, and resolves the initializer expression if present.
    /// Returns an error if the variable is declared more than once.
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

/// Represents a statement in the abstract syntax tree.
///
/// Statements include expression statements, return statements,
/// and empty (null) statements.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Null,
    Return(Expression),
    Expression(Expression),
}

impl Statement {
    /// Resolves identifiers within the statement using the provided symbol table.
    ///
    /// Recursively resolves any expressions contained in the statement.
    fn resolve(statement: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        let stmt = match statement {
            Self::Expression(expression) => {
                Self::Expression(Expression::resolve(expression, variables)?)
            }
            Self::Return(expression) => Self::Return(Expression::resolve(expression, variables)?),
            Self::Null => Self::Null,
        };
        Ok(stmt)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, ";"),
            Self::Expression(expression) => write!(f, "{};", expression),
            Self::Return(expression) => write!(f, "return {};", expression),
        }
    }
}

/// Represents an item within a block.
///
/// A block item may be either a variable declaration or a statement.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

impl BlockItem {
    /// Parses a single block item from the token stream.
    ///
    /// Block items include variable declarations, return statements,
    /// expression statements, and empty statements. Consumes the tokens
    /// required to form the item and returns an error if the input is malformed.
    fn parse(tokens: &mut VecDeque<tokenizer::Token>) -> Result<Self, String> {
        match tokens.pop_front() {
            Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                Ok(Self::Statement(Statement::Null))
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Int)) => {
                let name = match tokens.pop_front() {
                    Some(tokenizer::Token::Identifier(identifier)) => Ok(identifier),
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
                            tok => Err(format!("Expected ';' after declaration, found: {tok:?}")),
                        }
                    }
                    _ => Err(String::from("Expected ';' or '=' after variable name.")),
                }
            }
            Some(tokenizer::Token::Keyword(tokenizer::Keyword::Return)) => {
                let expression = Expression::parse(tokens, 0)?;
                let stmt = match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        Ok(Statement::Return(expression))
                    }
                    _ => Err(String::from("Expected ';' after return expression.")),
                };
                stmt.map(BlockItem::Statement)
            }
            Some(token) => {
                tokens.push_front(token);
                let expression = Expression::parse(tokens, 0)?;
                match tokens.pop_front() {
                    Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::Semicolon)) => {
                        Ok(BlockItem::Statement(Statement::Expression(expression)))
                    }
                    _ => Err(String::from("Expected ';' after expression statement.")),
                }
            }
            None => Err(String::from("Unexpected end of input while parsing body.")),
        }
    }

    /// Resolves identifiers within the block item using the provided symbol table.
    ///
    /// Variable declarations are registered and resolved, and statements
    /// are recursively resolved.
    fn resolve(item: Self, variables: &mut HashMap<String, String>) -> Result<Self, String> {
        match item {
            Self::Declaration(declaration) => {
                let declaration = Declaration::resolve(declaration, variables)?;
                Ok(Self::Declaration(declaration))
            }
            Self::Statement(stmt) => {
                let statement = Statement::resolve(stmt, variables)?;
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

/// Represents a function definition in the abstract syntax tree.
///
/// A function consists of a name and a sequence of block items forming
/// its body.
#[derive(Debug, Clone, PartialEq)]
pub struct Function(String, Vec<BlockItem>);

impl Function {
    /// Returns the function name.
    pub fn name(&self) -> &str {
        &self.0
    }

    /// Returns the sequence of block items that make up the function body.
    pub fn instructions(self) -> Vec<BlockItem> {
        self.1
    }

    /// Parses a function definition from the token stream.
    ///
    /// Expects a function signature of the form `int <name>(void)` followed
    /// by a block body. Consumes all tokens associated with the function and
    /// returns an error if the definition is malformed.
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

    /// Resolves identifiers within the function body using the provided symbol table.
    ///
    /// Recursively resolves all declarations and statements contained in the
    /// function body.
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

/// Represents a complete program.
///
/// A program consists of a single top-level function, which serves as
/// the entry point of the compilation unit.
#[derive(Debug, Clone, PartialEq)]
pub struct Program(Function);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Parses a complete program from a sequence of tokens.
///
/// Expects the input to contain exactly one valid function definition.
/// Returns an error if parsing fails or if extra tokens remain after
/// the program is parsed.
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

/// Validates and resolves the program's abstract syntax tree.
///
/// Performs identifier resolution and semantic checks, returning a
/// transformed program with resolved symbols or an error if validation fails.
pub fn validate(ast: Program) -> Result<Program, String> {
    let main = Function::from(ast);
    let mut vars: HashMap<String, String> = HashMap::new();
    Ok(Program(Function::resolve(main, &mut vars)?))
}
