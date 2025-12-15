use crate::tokens;

/// Represents an integer constant in the AST.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Integer(i64);

impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        Integer(value)
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

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier(name)
    }
}

impl From<Function> for Identifier {
    fn from(function: Function) -> Self {
        function.name
    }
}

impl From<Identifier> for String {
    fn from(identifier: Identifier) -> Self {
        identifier.0
    }
}

/// Represents unary operators in the AST.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    NEGATION,
    BITWISENOT,
}

impl From<tokens::Operator> for UnOp {
    fn from(op: tokens::Operator) -> Self {
        match op {
            tokens::Operator::NEGATION => UnOp::NEGATION,
            tokens::Operator::BITWISENOT => UnOp::BITWISENOT,
            tokens::Operator::DECREMENT => {
                panic!("DECREMENT operator is not supported in AST.")
            }
        }
    }
}

/// Represents an expression in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    INT(Integer),
    UNARY(UnOp, Box<Expression>),
}

impl From<i64> for Expression {
    fn from(value: i64) -> Self {
        Expression::INT(Integer::from(value))
    }
}

/// Represents a statement in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    RETURN(Expression),
}

impl From<Function> for Statement {
    fn from(function: Function) -> Self {
        function.body
    }
}

/// Represents a function in the AST.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: Identifier,
    body: Statement,
}

/// Methods for the Function struct.
impl Function {
    pub fn new(identifier: Identifier, statement: Statement) -> Self {
        Function {
            name: identifier,
            body: statement,
        }
    }
    pub fn body(&self) -> &Statement {
        &self.body
    }
    pub fn name(&self) -> &Identifier {
        &self.name
    }
}

impl From<Program> for Function {
    fn from(program: Program) -> Self {
        program.0
    }
}

/// Represents a complete program in the AST.
pub struct Program(Function);

impl From<Function> for Program {
    fn from(function: Function) -> Self {
        Program(function)
    }
}
