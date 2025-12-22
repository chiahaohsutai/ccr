use super::parser;
use nanoid::nanoid;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    COMPLEMENT,
    NEGATE,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::COMPLEMENT => write!(f, "~"),
            UnaryOperator::NEGATE => write!(f, "-"),
        }
    }
}

impl From<parser::UnaryOperator> for UnaryOperator {
    fn from(op: parser::UnaryOperator) -> Self {
        match op {
            parser::UnaryOperator::COMPLEMENT => UnaryOperator::COMPLEMENT,
            parser::UnaryOperator::NEGATE => UnaryOperator::NEGATE,
            _ => todo!(),
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
        }
    }
}

impl From<parser::BinaryOperator> for BinaryOperator {
    fn from(op: parser::BinaryOperator) -> Self {
        match op {
            parser::BinaryOperator::ADD => BinaryOperator::ADD,
            parser::BinaryOperator::SUBTRACT => BinaryOperator::SUBTRACT,
            parser::BinaryOperator::MULTIPLY => BinaryOperator::MULTIPLY,
            parser::BinaryOperator::DIVIDE => BinaryOperator::DIVIDE,
            parser::BinaryOperator::REMAINDER => BinaryOperator::REMAINDER,
            parser::BinaryOperator::BITWISEAND => BinaryOperator::BITWISEAND,
            parser::BinaryOperator::BITWISEOR => BinaryOperator::BITWISEOR,
            parser::BinaryOperator::BITWISEXOR => BinaryOperator::BITWISEXOR,
            parser::BinaryOperator::LEFTSHIFT => BinaryOperator::LEFTSHIFT,
            parser::BinaryOperator::RIGHTSHIFT => BinaryOperator::RIGHTSHIFT,
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    CONSTANT(u64),
    VARIABLE(String),
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
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::RETURN(v) => write!(f, "RETURN {}", v),
            Instruction::UNARY(op, src, dest) => write!(f, "{} {} {}", op, src, dest),
            Instruction::BINARY(op, l, r, dest) => write!(f, "{} {} {} {}", op, l, r, dest),
        }
    }
}

fn generate_instructions(
    expression: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    match expression {
        parser::Expression::FACTOR(factor) => match factor {
            parser::Factor::INT(n) => Operand::CONSTANT(n.into()),
            parser::Factor::UNARY(op, exp) => {
                let src = generate_instructions(parser::Expression::FACTOR(*exp), instructions);
                let dst = Operand::VARIABLE(format!("temp.{}", nanoid!(21)));
                let op = UnaryOperator::from(op);
                instructions.push(Instruction::UNARY(op, src, dst.clone()));
                dst
            }
            parser::Factor::EXPRESSION(expr) => generate_instructions(*expr, instructions),
        },
        parser::Expression::BINARY(lhs, op, rhs) => {
            let lhs = generate_instructions(*lhs, instructions);
            let rhs = generate_instructions(*rhs, instructions);
            let dst = Operand::VARIABLE(format!("temp.{}", nanoid!(21)));
            let op = BinaryOperator::from(op);
            instructions.push(Instruction::BINARY(op, lhs, rhs, dst.clone()));
            dst
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
        writeln!(f, "FUNCTION {}", self.0)?;
        for instr in &self.1 {
            writeln!(f, "  {}", instr)?;
        }
        writeln!(f, "END FUNCTION")
    }
}

impl From<Function> for Vec<Instruction> {
    fn from(function: Function) -> Self {
        function.1
    }
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        let name = String::from(function.name().as_ref());
        let mut instructions: Vec<Instruction> = Vec::new();
        match parser::Statement::from(function) {
            parser::Statement::RETURN(expr) => {
                let value = generate_instructions(expr, &mut instructions);
                instructions.push(Instruction::RETURN(value));
            }
        };
        Function(name, instructions)
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

impl From<parser::Program> for Program {
    fn from(program: parser::Program) -> Self {
        let function = parser::Function::from(program);
        Program(Function::from(function))
    }
}
