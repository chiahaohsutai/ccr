use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;

use std::fmt;

use super::parser;

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
    EQUAL,
    NOTEQUAL,
    LESSTHAN,
    GREATERTHAN,
    LESSEQUAL,
    GREATEREQUAL,
}

impl TryFrom<parser::BinaryOperator> for BinaryOperator {
    type Error = String;

    fn try_from(op: parser::BinaryOperator) -> Result<Self, String> {
        match op {
            parser::BinaryOperator::Add => Ok(Self::ADD),
            parser::BinaryOperator::Substract => Ok(Self::SUBTRACT),
            parser::BinaryOperator::Multiply => Ok(Self::MULTIPLY),
            parser::BinaryOperator::Divide => Ok(Self::DIVIDE),
            parser::BinaryOperator::Remainder => Ok(Self::REMAINDER),
            parser::BinaryOperator::BitwiseAnd => Ok(Self::BITWISEAND),
            parser::BinaryOperator::BitwiseOr => Ok(Self::BITWISEOR),
            parser::BinaryOperator::BitwiseXor => Ok(Self::BITWISEXOR),
            parser::BinaryOperator::LeftShift => Ok(Self::LEFTSHIFT),
            parser::BinaryOperator::RightShift => Ok(Self::RIGHTSHIFT),
            parser::BinaryOperator::EqualEqual => Ok(Self::EQUAL),
            parser::BinaryOperator::NotEqual => Ok(Self::NOTEQUAL),
            parser::BinaryOperator::LessThan => Ok(Self::LESSTHAN),
            parser::BinaryOperator::GreaterThan => Ok(Self::GREATERTHAN),
            parser::BinaryOperator::LessThanOrEq => Ok(Self::LESSEQUAL),
            parser::BinaryOperator::GreaterThanOrEq => Ok(Self::GREATEREQUAL),
            _ => Err(format!("Operator '{:?}' is not a tacky binary op.", op)),
        }
    }
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
            BinaryOperator::EQUAL => write!(f, "=="),
            BinaryOperator::NOTEQUAL => write!(f, "!="),
            BinaryOperator::LESSTHAN => write!(f, "<"),
            BinaryOperator::GREATERTHAN => write!(f, ">"),
            BinaryOperator::LESSEQUAL => write!(f, "<="),
            BinaryOperator::GREATEREQUAL => write!(f, ">="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    CONSTANT(u64),
    VARIABLE(String),
}

impl TryFrom<parser::Expression> for Operand {
    type Error = String;

    fn try_from(value: parser::Expression) -> Result<Self, String> {
        match value {
            parser::Expression::FACTOR(parser::Factor::Identifier(ident)) => {
                Ok(Self::VARIABLE(ident))
            }
            parser::Expression::FACTOR(parser::Factor::Int(i)) => Ok(Self::CONSTANT(i)),
            _ => Err(String::from("Expression is not an operand")),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::CONSTANT(c) => write!(f, "{}", c),
            Operand::VARIABLE(v) => write!(f, "{}", v),
        }
    }
}

pub enum Instruction {
    RETURN(Operand),
    UNARY(UnaryOperator, Operand, Operand),
    BINARY(BinaryOperator, Operand, Operand, Operand),
    COPY(Operand, Operand),
    JUMP(String),
    JUMPIF(Operand, String),
    JUMPIFNOT(Operand, String),
    LABEL(String),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::RETURN(v) => write!(f, "RETURN {}", v),
            Instruction::UNARY(op, src, dest) => write!(f, "{} {} {}", op, src, dest),
            Instruction::BINARY(op, l, r, dest) => write!(f, "{} {} {} {}", op, l, r, dest),
            Instruction::COPY(src, dest) => write!(f, "COPY {} {}", src, dest),
            Instruction::JUMP(label) => write!(f, "JUMP {}", label),
            Instruction::JUMPIF(cond, label) => write!(f, "JUMP IF ZERO {} {}", cond, label),
            Instruction::JUMPIFNOT(cond, label) => write!(f, "JUMP IF NOT ZERO {} {}", cond, label),
            Instruction::LABEL(label) => write!(f, "LABEL {}", label),
        }
    }
}

fn generate_instructions(
    expression: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    match expression {
        parser::Expression::FACTOR(factor) => match factor {
            parser::Factor::Int(n) => Ok(Operand::CONSTANT(n)),
            parser::Factor::Unary(op, exp) => {
                let src = generate_instructions(parser::Expression::FACTOR(*exp), instructions)?;
                let dst = Operand::VARIABLE(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));
                let op = UnaryOperator::from(op);
                instructions.push(Instruction::UNARY(op, src, dst.clone()));
                Ok(dst)
            }
            parser::Factor::Expression(expr) => generate_instructions(*expr, instructions),
            parser::Factor::Identifier(ident) => Ok(Operand::VARIABLE(ident)),
        },
        parser::Expression::BINARY(lhs, parser::BinaryOperator::Assignment, rhs) => {
            let rhs = generate_instructions(*rhs, instructions)?;
            let lhs = Operand::try_from(*lhs)?;
            instructions.push(Instruction::COPY(rhs, lhs.clone()));
            Ok(lhs)
        }
        parser::Expression::BINARY(lhs, op, rhs) => {
            let dst = Operand::VARIABLE(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));

            if let parser::BinaryOperator::LogicalAnd | parser::BinaryOperator::LogicalOr = op {
                let end = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                let lhs = generate_instructions(*lhs, instructions)?;

                let mut rhs_instructions: Vec<Instruction> = Vec::new();
                let rhs = generate_instructions(*rhs, &mut rhs_instructions)?;

                if matches!(op, parser::BinaryOperator::LogicalAnd) {
                    let isfalse = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                    instructions.push(Instruction::JUMPIF(lhs, String::from(&isfalse)));
                    instructions.extend(rhs_instructions);
                    instructions.push(Instruction::JUMPIF(rhs, String::from(&isfalse)));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(1), dst.clone()));
                    instructions.push(Instruction::JUMP(end.clone()));
                    instructions.push(Instruction::LABEL(isfalse));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(0), dst.clone()));
                    instructions.push(Instruction::LABEL(end));
                } else {
                    let istrue = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                    instructions.push(Instruction::JUMPIFNOT(lhs, String::from(&istrue)));
                    instructions.extend(rhs_instructions);
                    instructions.push(Instruction::JUMPIFNOT(rhs, String::from(&istrue)));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(0), dst.clone()));
                    instructions.push(Instruction::JUMP(end.clone()));
                    instructions.push(Instruction::LABEL(istrue));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(1), dst.clone()));
                    instructions.push(Instruction::LABEL(end));
                }
                Ok(dst)
            } else {
                let lhs = generate_instructions(*lhs, instructions)?;
                let rhs = generate_instructions(*rhs, instructions)?;
                let op = BinaryOperator::try_from(op).unwrap();
                instructions.push(Instruction::BINARY(op, lhs, rhs, dst.clone()));
                Ok(dst)
            }
        }
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
                        let dst = Operand::VARIABLE(name);
                        instructions.push(Instruction::COPY(opr, dst));
                    }
                }
                parser::BlockItem::Statement(stmt) => match stmt {
                    parser::Statement::EXPRESSION(expr) => {
                        let _ = generate_instructions(expr, &mut instructions)?;
                    }
                    parser::Statement::RETURN(expr) => {
                        let res = generate_instructions(expr, &mut instructions)?;
                        instructions.push(Instruction::RETURN(res));
                    }
                    parser::Statement::NULL => (),
                },
            };
        }
        match instructions.last() {
            Some(Instruction::RETURN(_)) => (),
            _ => instructions.push(Instruction::RETURN(Operand::CONSTANT(0))),
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
