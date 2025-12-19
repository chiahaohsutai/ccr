use core::fmt;
use std::collections::HashMap;

use super::tacky;

// Registers in x86-64 assembly.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Register {
    AX,
    DX,
    R10,
    R11,
}

// Operands in x86-64 assembly.
#[derive(Debug, Clone, PartialEq)]
enum Operand {
    IMM(u64),
    REG(Register),
    PSEUDO(String),
    STACK(u64),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::IMM(c) => write!(f, "${c}"),
            Operand::REG(r) => match r {
                Register::AX => write!(f, "%eax"),
                Register::R10 => write!(f, "%r10d"),
                Register::DX => write!(f, "%edx"),
                Register::R11 => write!(f, "%r11d"),
            },
            Operand::PSEUDO(name) => panic!("Unexpected pseudo operand: {}", name),
            Operand::STACK(offset) => write!(f, "-{}(%rbp)", offset),
        }
    }
}

impl From<tacky::Operand> for Operand {
    fn from(operand: tacky::Operand) -> Self {
        match operand {
            tacky::Operand::CONSTANT(c) => Operand::IMM(c),
            tacky::Operand::VARIABLE(v) => Operand::PSEUDO(v),
        }
    }
}

// Unary operators in x86-64 assembly.
#[derive(Debug, Clone, Copy, PartialEq)]
enum UnaryOperator {
    NEG,
    NOT,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::NEG => write!(f, "negl"),
            UnaryOperator::NOT => write!(f, "notl"),
        }
    }
}

impl From<tacky::UnaryOperator> for UnaryOperator {
    fn from(op: tacky::UnaryOperator) -> Self {
        match op {
            tacky::UnaryOperator::NEGATE => UnaryOperator::NEG,
            tacky::UnaryOperator::COMPLEMENT => UnaryOperator::NOT,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BinaryOperator {
    ADD,
    SUB,
    MUL,
}

// Instructions in x86-64 assembly.
#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    MOV(Operand, Operand),
    UNARY(UnaryOperator, Operand),
    BINARY(BinaryOperator, Operand, Operand),
    IDIV(Operand),
    CDQ,
    ALLOCATE(u64),
    RET,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::MOV(src, dest) => write!(f, "\tmovl {src}, {dest}"),
            Instruction::ALLOCATE(size) => write!(f, "\tsubq ${size}, %rsp"),
            Instruction::UNARY(op, operand) => write!(f, "\t{op} {operand}"),
            Instruction::RET => write!(f, "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Instruction::CDQ => write!(f, "\tcdq"),
            Instruction::IDIV(operand) => write!(f, "\tidivl {operand}"),
            Instruction::BINARY(op, lhs, rhs) => {
                let op_str = match op {
                    BinaryOperator::ADD => "addl",
                    BinaryOperator::SUB => "subl",
                    BinaryOperator::MUL => "imull",
                };
                write!(f, "\t{} {}, {}", op_str, lhs, rhs)
            }
        }
    }
}

struct StackOffsets {
    offsets: HashMap<String, u64>,
    current_offset: u64,
}

impl Default for StackOffsets {
    fn default() -> Self {
        StackOffsets {
            offsets: HashMap::new(),
            current_offset: 0,
        }
    }
}

impl StackOffsets {
    fn get(&mut self, name: &str) -> u64 {
        if let Some(offset) = self.offsets.get(name) {
            *offset
        } else {
            self.current_offset += 4;
            self.offsets.insert(String::from(name), self.current_offset);
            self.current_offset
        }
    }
    fn size(&self) -> u64 {
        self.current_offset
    }
}

// Represents a function in x86-64 assembly.
#[derive(Debug, Clone, PartialEq)]
struct Function(String, Vec<Instruction>);

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lines: Vec<String> = vec![
            format!("\t.globl _{}", self.0),
            format!("_{}:", self.0),
            String::from("\tpushq %rbp"),
            String::from("\tmovq %rsp, %rbp"),
        ];
        let lines = self.1.iter().fold(lines, |mut acc, instruction| {
            acc.push(instruction.to_string());
            acc
        });
        write!(f, "{}", lines.join("\n"))
    }
}

/// Generate pseudo-instructions for a given tacky instruction.
fn generate_pseudo_instructions(instruction: tacky::Instruction) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    match instruction {
        tacky::Instruction::RETURN(operand) => {
            let op = Operand::from(operand);
            instructions.push(Instruction::MOV(op, Operand::REG(Register::AX)));
            instructions.push(Instruction::RET);
        }
        tacky::Instruction::UNARY(op, src, dest) => {
            let operator = UnaryOperator::from(op);
            let source = Operand::from(src);
            let destination = Operand::from(dest);
            instructions.push(Instruction::MOV(source, destination.clone()));
            instructions.push(Instruction::UNARY(operator, destination));
        }
        tacky::Instruction::BINARY(op, lhs, rhs, dest) => match op {
            tacky::BinaryOperator::DIVIDE => {
                let left = Operand::from(lhs);
                let right = Operand::from(rhs);
                let destination = Operand::from(dest);
                instructions.push(Instruction::MOV(left, Operand::REG(Register::AX)));
                instructions.push(Instruction::CDQ);
                instructions.push(Instruction::IDIV(right));
                instructions.push(Instruction::MOV(Operand::REG(Register::AX), destination));
            }
            tacky::BinaryOperator::REMAINDER => {
                let left = Operand::from(lhs);
                let right = Operand::from(rhs);
                let destination = Operand::from(dest);
                instructions.push(Instruction::MOV(left, Operand::REG(Register::AX)));
                instructions.push(Instruction::CDQ);
                instructions.push(Instruction::IDIV(right));
                instructions.push(Instruction::MOV(Operand::REG(Register::DX), destination));
            }
            _ => {
                instructions.push(Instruction::MOV(
                    Operand::from(lhs),
                    Operand::from(dest.clone()),
                ));
                let op = match op {
                    tacky::BinaryOperator::ADD => BinaryOperator::ADD,
                    tacky::BinaryOperator::SUBTRACT => BinaryOperator::SUB,
                    tacky::BinaryOperator::MULTIPLY => BinaryOperator::MUL,
                    _ => unreachable!(),
                };
                instructions.push(Instruction::BINARY(
                    op,
                    Operand::from(rhs),
                    Operand::from(dest),
                ));
            }
        },
    };
    instructions
}

// Replace pseudo-operands with stack offsets.
fn replace_pseudo_operands(instruction: Instruction, offsets: &mut StackOffsets) -> Instruction {
    match instruction {
        Instruction::MOV(Operand::PSEUDO(n1), Operand::PSEUDO(n2)) => {
            let offset1 = offsets.get(&n1);
            let offset2 = offsets.get(&n2);
            Instruction::MOV(Operand::STACK(offset1), Operand::STACK(offset2))
        }
        Instruction::MOV(op, Operand::PSEUDO(n)) => {
            Instruction::MOV(op, Operand::STACK(offsets.get(&n)))
        }
        Instruction::MOV(Operand::PSEUDO(n), op) => {
            Instruction::MOV(Operand::STACK(offsets.get(&n)), op)
        }
        Instruction::UNARY(op, Operand::PSEUDO(name)) => {
            Instruction::UNARY(op, Operand::STACK(offsets.get(&name)))
        }
        Instruction::BINARY(op, Operand::PSEUDO(n1), Operand::PSEUDO(n2)) => Instruction::BINARY(
            op,
            Operand::STACK(offsets.get(&n1)),
            Operand::STACK(offsets.get(&n2)),
        ),
        Instruction::BINARY(op, lhs, Operand::PSEUDO(name)) => {
            Instruction::BINARY(op, lhs, Operand::STACK(offsets.get(&name)))
        }
        Instruction::BINARY(op, Operand::PSEUDO(name), rhs) => {
            Instruction::BINARY(op, Operand::STACK(offsets.get(&name)), rhs)
        }
        Instruction::IDIV(Operand::PSEUDO(name)) => {
            Instruction::IDIV(Operand::STACK(offsets.get(&name)))
        }
        _ => instruction,
    }
}

// Fix invalid instructions that may arise during translation.
fn fix_invalid_instruction(instruction: Instruction) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    match instruction {
        Instruction::MOV(Operand::STACK(n1), Operand::STACK(n2)) => {
            instructions.push(Instruction::MOV(
                Operand::STACK(n1),
                Operand::REG(Register::R10),
            ));
            instructions.push(Instruction::MOV(
                Operand::REG(Register::R10),
                Operand::STACK(n2),
            ));
        }
        Instruction::BINARY(BinaryOperator::ADD, Operand::STACK(n1), Operand::STACK(n2)) => {
            instructions.push(Instruction::MOV(
                Operand::STACK(n1),
                Operand::REG(Register::R10),
            ));
            instructions.push(Instruction::BINARY(
                BinaryOperator::ADD,
                Operand::REG(Register::R10),
                Operand::STACK(n2),
            ));
        }
        Instruction::BINARY(BinaryOperator::SUB, Operand::STACK(n1), Operand::STACK(n2)) => {
            instructions.push(Instruction::MOV(
                Operand::STACK(n1),
                Operand::REG(Register::R10),
            ));
            instructions.push(Instruction::BINARY(
                BinaryOperator::SUB,
                Operand::REG(Register::R10),
                Operand::STACK(n2),
            ));
        }
        Instruction::BINARY(BinaryOperator::MUL, lhs, Operand::STACK(n)) => {
            instructions.push(Instruction::MOV(
                Operand::STACK(n),
                Operand::REG(Register::R11),
            ));
            instructions.push(Instruction::BINARY(
                BinaryOperator::MUL,
                lhs,
                Operand::REG(Register::R11),
            ));
            instructions.push(Instruction::MOV(
                Operand::REG(Register::R11),
                Operand::STACK(n),
            ));
        }
        Instruction::IDIV(Operand::IMM(v)) => {
            instructions.push(Instruction::MOV(
                Operand::IMM(v),
                Operand::REG(Register::R10),
            ));
            instructions.push(Instruction::IDIV(Operand::REG(Register::R10)));
        }
        _ => instructions.push(instruction),
    };
    instructions
}

impl From<tacky::Function> for Function {
    fn from(func: tacky::Function) -> Self {
        let name = String::from(func.name());

        let mut instructions: Vec<Instruction> = Vec::new();
        instructions.push(Instruction::ALLOCATE(0));

        for instr in Vec::from(func) {
            let asm_instructions = generate_pseudo_instructions(instr);
            instructions.extend(asm_instructions);
        }

        let mut offsets = StackOffsets::default();
        let mut instructions = instructions
            .into_iter()
            .map(|instr| replace_pseudo_operands(instr, &mut offsets))
            .collect::<Vec<Instruction>>();

        let size = offsets.size();
        instructions[0] = Instruction::ALLOCATE(size);

        let instructions = instructions
            .into_iter()
            .flat_map(|instr| fix_invalid_instruction(instr))
            .collect::<Vec<Instruction>>();

        Function(name, instructions)
    }
}

// Represents a program in x86-64 assembly.
#[derive(Debug, Clone, PartialEq)]
pub struct Program(Function);

impl From<tacky::Program> for Program {
    fn from(program: tacky::Program) -> Self {
        let function = tacky::Function::from(program);
        Program(Function::from(function))
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
