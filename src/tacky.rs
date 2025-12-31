use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;

use std::fmt;

use crate::parser::Expression;

use super::parser;

/// Represents unary operators that operate on a single operand.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negation,
    LogicalNot,
}

impl From<parser::UnaryOperator> for UnaryOperator {
    fn from(op: parser::UnaryOperator) -> Self {
        match op {
            parser::UnaryOperator::Complement => Self::Complement,
            parser::UnaryOperator::Negation => Self::Negation,
            parser::UnaryOperator::LogicalNot => Self::LogicalNot,
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Complement => write!(f, "~"),
            Self::Negation => write!(f, "-"),
            Self::LogicalNot => write!(f, "!"),
        }
    }
}

/// Represents binary operators that operate on two operands.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
}

impl TryFrom<parser::BinaryOperator> for BinaryOperator {
    type Error = String;

    fn try_from(op: parser::BinaryOperator) -> Result<Self, String> {
        match op {
            parser::BinaryOperator::Add => Ok(Self::Add),
            parser::BinaryOperator::Substract => Ok(Self::Subtract),
            parser::BinaryOperator::Multiply => Ok(Self::Multiply),
            parser::BinaryOperator::Divide => Ok(Self::Divide),
            parser::BinaryOperator::Remainder => Ok(Self::Remainder),
            parser::BinaryOperator::BitwiseAnd => Ok(Self::BitwiseAnd),
            parser::BinaryOperator::BitwiseOr => Ok(Self::BitwiseOr),
            parser::BinaryOperator::BitwiseXor => Ok(Self::BitwiseXor),
            parser::BinaryOperator::LeftShift => Ok(Self::LeftShift),
            parser::BinaryOperator::RightShift => Ok(Self::RightShift),
            parser::BinaryOperator::EqualEqual => Ok(Self::EqualEqual),
            parser::BinaryOperator::NotEqual => Ok(Self::NotEqual),
            parser::BinaryOperator::LessThan => Ok(Self::LessThan),
            parser::BinaryOperator::GreaterThan => Ok(Self::GreaterThan),
            parser::BinaryOperator::LessThanOrEq => Ok(Self::LessThanOrEq),
            parser::BinaryOperator::GreaterThanOrEq => Ok(Self::GreaterThanOrEq),
            _ => Err(format!("Operator '{:?}' is not a tacky binary op.", op)),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Remainder => write!(f, "%"),
            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseXor => write!(f, "^"),
            Self::LeftShift => write!(f, "<<"),
            Self::RightShift => write!(f, ">>"),
            Self::EqualEqual => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanOrEq => write!(f, "<="),
            Self::GreaterThanOrEq => write!(f, ">="),
        }
    }
}

/// Represents an operand in an expression.
///
/// An operand may be a constant literal or a variable reference.
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Constant(u64),
    Variable(String),
}

impl TryFrom<parser::Expression> for Operand {
    type Error = String;

    fn try_from(value: parser::Expression) -> Result<Self, String> {
        match value {
            parser::Expression::Factor(parser::Factor::Identifier(ident)) => {
                Ok(Self::Variable(ident))
            }
            parser::Expression::Factor(parser::Factor::Int(i)) => Ok(Self::Constant(i)),
            _ => Err(String::from("Expression is not an operand")),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Variable(v) => write!(f, "{}", v),
        }
    }
}

/// Represents a low-level instruction in the intermediate representation.
///
/// Instructions operate on operands and model control flow, data movement,
/// and arithmetic or logical operations after semantic analysis.
pub enum Instruction {
    Return(Operand),
    Unary(UnaryOperator, Operand, Operand),
    Binary(BinaryOperator, Operand, Operand, Operand),
    Copy(Operand, Operand),
    Jump(String),
    JumpIfZero(Operand, String),
    JumpIfNotZero(Operand, String),
    Label(String),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return(v) => write!(f, "Return {}", v),
            Instruction::Unary(op, src, dest) => write!(f, "{} {} {}", op, src, dest),
            Instruction::Binary(op, l, r, dest) => write!(f, "{} {} {} {}", op, l, r, dest),
            Instruction::Copy(src, dest) => write!(f, "Copy {} {}", src, dest),
            Instruction::Jump(label) => write!(f, "Jump {}", label),
            Instruction::JumpIfZero(cond, label) => write!(f, "Jump If Zero {} {}", cond, label),
            Instruction::JumpIfNotZero(cond, label) => {
                write!(f, "Jump If Not Zero {} {}", cond, label)
            }
            Instruction::Label(label) => write!(f, "Label {}", label),
        }
    }
}

/// Generates intermediate instructions for a binary expression.
///
/// Lowers a binary expression AST node into IR instructions, appending them
/// to `instructions`, and returns the operand containing the result. Handles
/// assignments, compound assignments, and preserves short-circuit semantics
/// for logical operators.
fn generate_binary_expression_instructions(
    expression: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    match expression {
        Expression::Binary(lhs, op, rhs) => match op {
            parser::BinaryOperator::Assignment => {
                let rhs = generate_instructions(*rhs, instructions)?;
                let lhs = Operand::try_from(*lhs)?;
                instructions.push(Instruction::Copy(rhs, lhs.clone()));
                Ok(lhs)
            }
            parser::BinaryOperator::LogicalAnd | parser::BinaryOperator::LogicalOr => {
                let dst = Operand::Variable(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));

                let end = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                let lhs = generate_instructions(*lhs, instructions)?;

                let mut rhs_instructions: Vec<Instruction> = Vec::new();
                let rhs = generate_instructions(*rhs, &mut rhs_instructions)?;

                if matches!(op, parser::BinaryOperator::LogicalAnd) {
                    let isfalse = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                    instructions.push(Instruction::JumpIfZero(lhs, String::from(&isfalse)));
                    instructions.extend(rhs_instructions);
                    instructions.push(Instruction::JumpIfZero(rhs, String::from(&isfalse)));
                    instructions.push(Instruction::Copy(Operand::Constant(1), dst.clone()));
                    instructions.push(Instruction::Jump(end.clone()));
                    instructions.push(Instruction::Label(isfalse));
                    instructions.push(Instruction::Copy(Operand::Constant(0), dst.clone()));
                    instructions.push(Instruction::Label(end));
                } else {
                    let istrue = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                    instructions.push(Instruction::JumpIfNotZero(lhs, String::from(&istrue)));
                    instructions.extend(rhs_instructions);
                    instructions.push(Instruction::JumpIfNotZero(rhs, String::from(&istrue)));
                    instructions.push(Instruction::Copy(Operand::Constant(0), dst.clone()));
                    instructions.push(Instruction::Jump(end.clone()));
                    instructions.push(Instruction::Label(istrue));
                    instructions.push(Instruction::Copy(Operand::Constant(1), dst.clone()));
                    instructions.push(Instruction::Label(end));
                }
                Ok(dst)
            }
            op if op.is_assignment() => {
                let rhs = generate_instructions(*rhs, instructions)?;
                let op = match op {
                    parser::BinaryOperator::AddAssignment => Ok(BinaryOperator::Add),
                    parser::BinaryOperator::SubAssignment => Ok(BinaryOperator::Subtract),
                    parser::BinaryOperator::DivAssignment => Ok(BinaryOperator::Divide),
                    parser::BinaryOperator::RemAssignment => Ok(BinaryOperator::Remainder),
                    parser::BinaryOperator::ProdAssignment => Ok(BinaryOperator::Multiply),
                    parser::BinaryOperator::AndAssignment => Ok(BinaryOperator::BitwiseAnd),
                    parser::BinaryOperator::OrAssignment => Ok(BinaryOperator::BitwiseOr),
                    parser::BinaryOperator::XorAssignment => Ok(BinaryOperator::BitwiseXor),
                    parser::BinaryOperator::LShiftAssignment => Ok(BinaryOperator::LeftShift),
                    parser::BinaryOperator::RShiftAssignment => Ok(BinaryOperator::RightShift),
                    _ => Err(format!("Expected compound asssignment op, found: {op}")),
                }?;
                let temp = Operand::Variable(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));
                let lhs = generate_instructions(*lhs, instructions)?;
                instructions.push(Instruction::Binary(op, lhs.clone(), rhs, temp.clone()));
                instructions.push(Instruction::Copy(temp, lhs.clone()));
                Ok(lhs)
            }
            op => {
                let dst = Operand::Variable(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));
                let lhs = generate_instructions(*lhs, instructions)?;
                let rhs = generate_instructions(*rhs, instructions)?;
                let op = BinaryOperator::try_from(op).unwrap();
                instructions.push(Instruction::Binary(op, lhs, rhs, dst.clone()));
                Ok(dst)
            }
        },
        _ => Err(format!("Expected binary expression, found: {}", expression)),
    }
}

/// Generates intermediate instructions for the given expression.
///
/// Recursively lowers an expression AST into a sequence of IR instructions,
/// appending them to `instructions`. Returns the operand that holds the
/// expression’s resulting value.
///
/// Temporary variables are introduced as needed, and short-circuit semantics
/// are preserved for logical operators.
fn generate_instructions(
    expression: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    if let parser::Expression::Factor(factor) = expression {
        match factor {
            parser::Factor::Int(n) => Ok(Operand::Constant(n)),
            parser::Factor::Unary(op, exp) => {
                let src = generate_instructions(parser::Expression::Factor(*exp), instructions)?;
                let dst = Operand::Variable(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));
                let op = UnaryOperator::from(op);
                instructions.push(Instruction::Unary(op, src, dst.clone()));
                Ok(dst)
            }
            parser::Factor::Expression(expr) => generate_instructions(*expr, instructions),
            parser::Factor::Identifier(ident) => Ok(Operand::Variable(ident)),
        }
    } else {
        generate_binary_expression_instructions(expression, instructions)
    }
}

pub struct Function(String, Vec<Instruction>);

impl Function {
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let instrs = self
            .1
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join("\n\t");
        writeln!(f, "FN {}\n\t{}\nEND FN {}", self.0, instrs, self.0)
    }
}

impl From<Function> for Vec<Instruction> {
    fn from(function: Function) -> Self {
        function.1
    }
}

impl TryFrom<parser::Function> for Function {
    type Error = String;

    fn try_from(function: parser::Function) -> Result<Self, String> {
        let name = String::from(function.name());
        let mut instructions: Vec<Instruction> = Vec::new();
        for item in function.instructions() {
            match item {
                parser::BlockItem::Declaration(decl) => {
                    let name = String::from(decl.name());
                    if let Some(expr) = decl.initializer() {
                        let opr = generate_instructions(expr, &mut instructions)?;
                        let dst = Operand::Variable(name);
                        instructions.push(Instruction::Copy(opr, dst));
                    }
                }
                parser::BlockItem::Statement(stmt) => match stmt {
                    parser::Statement::Expression(expr) => {
                        let _ = generate_instructions(expr, &mut instructions)?;
                    }
                    parser::Statement::Return(expr) => {
                        let res = generate_instructions(expr, &mut instructions)?;
                        instructions.push(Instruction::Return(res));
                    }
                    parser::Statement::Null => (),
                },
            };
        }
        match instructions.last() {
            Some(Instruction::Return(_)) => (),
            _ => instructions.push(Instruction::Return(Operand::Constant(0))),
        };
        Ok(Function(name, instructions))
    }
}

impl From<Program> for Function {
    fn from(program: Program) -> Self {
        program.0
    }
}

pub struct Program(Function);

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TryFrom<parser::Program> for Program {
    type Error = String;

    fn try_from(program: parser::Program) -> Result<Self, String> {
        let function = parser::Function::from(program);
        Ok(Program(Function::try_from(function)?))
    }
}
