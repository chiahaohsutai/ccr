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
}

#[derive(Debug, Clone, PartialEq)]
struct Function(String, Vec<Instruction>);

impl From<tacky::Function> for Function {
    fn from(func: tacky::Function) -> Self {
        let name = String::from(func.as_ref());
        let mut instructions: Vec<Instruction> = Vec::new();

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
        let instructions = instructions
            .into_iter()
            .map(|instruction| match instruction {
                Instruction::Mov(op, Operand::PSEUDO(n)) => {
                    let offset = offsets.get(&n);
                    Instruction::Mov(op, Operand::STACK(offset))
                }
                Instruction::Unary(op, Operand::PSEUDO(name)) => {
                    let offset = offsets.get(&name);
                    Instruction::Unary(op, Operand::STACK(offset))
                }
                _ => instruction,
            })
            .collect::<Vec<Instruction>>();

        Function(name, instructions)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Program(Function);

impl From<tacky::Program> for Program {
    fn from(program: tacky::Program) -> Self {
        let function = tacky::Function::from(program);
        Program(Function::from(function))
    }
}
