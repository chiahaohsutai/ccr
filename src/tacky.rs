use super::parser;
use nanoid::nanoid;
use std::fmt;

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
        }
    }
}

pub enum Operand {
    CONSTANT(i64),
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
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::RETURN(v) => write!(f, "RETURN {}", v),
            Instruction::UNARY(op, src, dest) => write!(f, "{} {} {}", op, src, dest),
        }
    }
}

fn generate_instructions(
    expression: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    match expression {
        parser::Expression::INT(n) => Operand::CONSTANT(n.into()),
        parser::Expression::UNARY(op, exp) => {
            let src = generate_instructions(*exp, instructions);
            let dst = format!("temp.{}", nanoid!(21));
            instructions.push(Instruction::UNARY(
                UnaryOperator::from(op),
                src,
                Operand::VARIABLE(String::from(&dst)),
            ));
            Operand::VARIABLE(dst)
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
