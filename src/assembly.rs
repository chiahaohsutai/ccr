use crate::parser::{self, Identifier};
use std::fmt;

/// Represents an operation's operand in assembly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operand {
    Register,
    IMM(i64),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Register => write!(f, "%eax"),
            Operand::IMM(int) => write!(f, "${}", int),
        }
    }
}

impl From<parser::Expression> for Operand {
    fn from(expression: parser::Expression) -> Self {
        match expression {
            parser::Expression::INT(integer) => Operand::IMM(integer.into()),
        }
    }
}

/// Represents a MOV instruction in assembly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Mov {
    src: Operand,
    dst: Operand,
}

impl Mov {
    fn new(src: Operand, dst: Operand) -> Self {
        Mov { src, dst }
    }
}

impl fmt::Display for Mov {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "movl {}, {}", self.src, self.dst)
    }
}

/// Represents an instruction in assembly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    MOV(Mov),
    RET,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::MOV(mov) => write!(f, "\t{mov}\n"),
            Instruction::RET => write!(f, "\tret\n"),
        }
    }
}

/// Represents a function in assembly.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Function {
    name: String,
    instructions: Vec<Instruction>,
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        let mut instructions: Vec<Instruction> = Vec::new();
        match function.body() {
            &parser::Statement::RETURN(expression) => {
                let mov = Mov::new(Operand::from(expression), Operand::Register);
                instructions.push(Instruction::MOV(mov));
                instructions.push(Instruction::RET);
            }
        };
        let name = String::from(Identifier::from(function));
        Function { name, instructions }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let header = format!("\t.globl _{}\n_{}:\n", self.name, self.name);
        let instructions = &self
            .instructions
            .iter()
            .map(|instruction| instruction.to_string())
            .collect::<String>();

        write!(f, "{header}{instructions}")
    }
}

/// Represents a complete assembly program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program(Function);

impl From<Function> for Program {
    fn from(function: Function) -> Self {
        Program(function)
    }
}

impl From<parser::Program> for Program {
    fn from(program: parser::Program) -> Self {
        let function: parser::Function = program.into();
        Program(Function::from(function))
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
