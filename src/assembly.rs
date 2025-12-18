use core::fmt;
use std::collections::HashMap;

use super::tacky;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Register {
    AX,
    R10,
}

#[derive(Debug, Clone, PartialEq)]
enum Operand {
    IMM(i64),
    REG(Register),
    PSEUDO(String),
    STACK(i64),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::IMM(c) => write!(f, "${c}"),
            Operand::REG(r) => match r {
                Register::AX => write!(f, "%eax"),
                Register::R10 => write!(f, "%r10d"),
            },
            Operand::PSEUDO(name) => panic!("Unexpected pseudo operand: {}", name),
            Operand::STACK(offset) => write!(f, "{}(%rbp)", offset),
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

#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    AllocateStack(i64),
    Ret,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(src, dest) => write!(f, "\tmovl {src}, {dest}"),
            Instruction::AllocateStack(size) => write!(f, "\tsubq ${size}, %rsp"),
            Instruction::Unary(op, operand) => write!(f, "\t{op} {operand}"),
            Instruction::Ret => write!(f, "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"),
        }
    }
}

struct StackOffsets {
    offsets: HashMap<String, i64>,
    current_offset: i64,
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
    fn get(&mut self, name: &str) -> i64 {
        if let Some(offset) = self.offsets.get(name) {
            *offset
        } else {
            self.current_offset -= 4;
            self.offsets.insert(String::from(name), self.current_offset);
            self.current_offset
        }
    }
    fn size(&self) -> i64 {
        -self.current_offset
    }
}

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

impl From<tacky::Function> for Function {
    fn from(func: tacky::Function) -> Self {
        let name = String::from(func.name());

        let mut instructions: Vec<Instruction> = Vec::new();
        instructions.push(Instruction::AllocateStack(0));

        Vec::from(func)
            .into_iter()
            .for_each(|instruction| match instruction {
                tacky::Instruction::RETURN(operand) => {
                    let op = Operand::from(operand);
                    instructions.push(Instruction::Mov(op, Operand::REG(Register::AX)));
                    instructions.push(Instruction::Ret);
                }
                tacky::Instruction::UNARY(op, src, dest) => {
                    let operator = UnaryOperator::from(op);
                    let source = Operand::from(src);
                    let destination = Operand::from(dest);
                    instructions.push(Instruction::Mov(source, destination.clone()));
                    instructions.push(Instruction::Unary(operator, destination));
                }
            });

        let mut offsets = StackOffsets::default();
        let mut instructions = instructions
            .into_iter()
            .map(|instruction| match instruction {
                Instruction::Mov(Operand::PSEUDO(n1), Operand::PSEUDO(n2)) => {
                    let offset1 = offsets.get(&n1);
                    let offset2 = offsets.get(&n2);
                    Instruction::Mov(Operand::STACK(offset1), Operand::STACK(offset2))
                }
                Instruction::Mov(op, Operand::PSEUDO(n)) => {
                    Instruction::Mov(op, Operand::STACK(offsets.get(&n)))
                }
                Instruction::Mov(Operand::PSEUDO(n), op) => {
                    Instruction::Mov(Operand::STACK(offsets.get(&n)), op)
                }
                Instruction::Unary(op, Operand::PSEUDO(name)) => {
                    Instruction::Unary(op, Operand::STACK(offsets.get(&name)))
                }
                _ => instruction,
            })
            .collect::<Vec<Instruction>>();

        let size = offsets.size();
        instructions[0] = Instruction::AllocateStack(size);

        let mut final_instructions: Vec<Instruction> = Vec::new();
        instructions
            .into_iter()
            .for_each(|instruction| match instruction {
                Instruction::Mov(Operand::STACK(n1), Operand::STACK(n2)) => {
                    final_instructions.push(Instruction::Mov(
                        Operand::STACK(n1),
                        Operand::REG(Register::R10),
                    ));
                    final_instructions.push(Instruction::Mov(
                        Operand::REG(Register::R10),
                        Operand::STACK(n2),
                    ));
                }
                _ => final_instructions.push(instruction),
            });

        Function(name, final_instructions)
    }
}

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
