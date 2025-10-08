use crate::parser::{self, Identifier};

// program             = Program(function_definition)
// function_definition = Function(identifier name, instruction* instructions)
// instruction         = Mov(operand src, operand dst) | Ret
// operand             = Imm(int) | Register

// AST node - Assembly construct
// Program(function_definition) - Program(function_definition)
// Function(name, body)         - Function(name, instructions)
// Return(exp)    (Stmt)        - Mov(exp, Register) Ret
// Constant(int)  (Exp)         - Imm(int)

/// Represents an operation's operand in assembly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operand {
    Register,
    IMM(i64),
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

/// Represents an instruction in assembly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    MOV(Mov),
    RET,
}

/// Represents a function in assembly.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Function {
    name: String, 
    instructions: Vec<Instruction> 
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