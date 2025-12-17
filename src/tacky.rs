use super::parser;
use nanoid::nanoid;
use std::fmt;

enum UnaryOp {
    COMPLEMENT,
    NEGATION,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::COMPLEMENT => write!(f, "~"),
            UnaryOp::NEGATION => write!(f, "-"),
        }
    }
}

impl From<parser::UnaryOp> for UnaryOp {
    fn from(op: parser::UnaryOp) -> Self {
        match op {
            parser::UnaryOp::BITWISENOT => UnaryOp::COMPLEMENT,
            parser::UnaryOp::NEGATION => UnaryOp::NEGATION,
        }
    }
}

enum Value {
    CONSTANT(i64),
    VARIABLE(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::CONSTANT(c) => write!(f, "{}", c),
            Value::VARIABLE(v) => write!(f, "{}", v),
        }
    }
}

enum Instruction {
    RETURN(Value),
    UNARY(UnaryOp, Value, Value),
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
) -> Value {
    match expression {
        parser::Expression::INT(n) => Value::CONSTANT(n.into()),
        parser::Expression::UNARY(op, exp) => {
            let src = generate_instructions(*exp, instructions);
            let dst = format!("temp.{}", nanoid!(21));
            instructions.push(Instruction::UNARY(
                UnaryOp::from(op),
                src,
                Value::VARIABLE(String::from(&dst)),
            ));
            Value::VARIABLE(dst)
        }
    }
}

struct Function(String, Vec<Instruction>);

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FUNCTION {}", self.0)?;
        for instr in &self.1 {
            writeln!(f, "  {}", instr)?;
        }
        writeln!(f, "END FUNCTION")
    }
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        let name = String::from(function.as_ref());
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
