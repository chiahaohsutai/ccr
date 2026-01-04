use core::fmt;
use std::collections::HashMap;

use super::tacky;

/// Represents a physical CPU register used during code generation.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Register {
    AX,
    DX,
    CX,
    R10,
    R11,
}

/// Returns the byte-sized register name for this register.
///
/// This is used when emitting instructions that operate on 8-bit values.
impl Register {
    fn as_byte(&self) -> String {
        match self {
            Self::AX => String::from("%al"),
            Self::CX => String::from("%cl"),
            Self::DX => String::from("%dl"),
            Self::R10 => String::from("%r10b"),
            Self::R11 => String::from("%r11b"),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AX => write!(f, "%eax"),
            Self::CX => write!(f, "%ecx"),
            Self::DX => write!(f, "%edx"),
            Self::R10 => write!(f, "%r10d"),
            Self::R11 => write!(f, "%r11d"),
        }
    }
}

/// Represents an operand used during code generation.
///
/// Operands may be immediate values, physical registers, stack locations,
/// or pseudo-registers introduced during intermediate lowering.
#[derive(Debug, Clone, PartialEq)]
enum Operand {
    Imm(u64),
    Reg(Register),
    Pseudo(String),
    Stack(u64),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Imm(c) => write!(f, "${c}"),
            Self::Reg(r) => write!(f, "{}", r),
            Self::Pseudo(name) => panic!("Unexpected pseudo operand: {}", name),
            Self::Stack(offset) => write!(f, "-{}(%rbp)", offset),
        }
    }
}

impl From<tacky::Operand> for Operand {
    fn from(operand: tacky::Operand) -> Self {
        match operand {
            tacky::Operand::Constant(c) => Self::Imm(c),
            tacky::Operand::Variable(v) => Self::Pseudo(v),
        }
    }
}

/// Represents unary operations supported by the code generator.
#[derive(Debug, Clone, Copy, PartialEq)]
enum UnaryOperator {
    Neg,
    Not,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "negl"),
            Self::Not => write!(f, "notl"),
        }
    }
}

impl TryFrom<tacky::UnaryOperator> for UnaryOperator {
    type Error = String;

    fn try_from(op: tacky::UnaryOperator) -> Result<Self, String> {
        match op {
            tacky::UnaryOperator::Negation => Ok(Self::Neg),
            tacky::UnaryOperator::Complement => Ok(Self::Not),
            op => Err(format!("Inavalid assembly operator: {op}")),
        }
    }
}

/// Represents binary operations supported by the code generator.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    Shl,
    Sar,
}

impl TryFrom<tacky::BinaryOperator> for BinaryOperator {
    type Error = String;

    fn try_from(op: tacky::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            tacky::BinaryOperator::Add => Ok(Self::Add),
            tacky::BinaryOperator::Subtract => Ok(Self::Sub),
            tacky::BinaryOperator::Multiply => Ok(Self::Mul),
            tacky::BinaryOperator::BitwiseAnd => Ok(Self::And),
            tacky::BinaryOperator::BitwiseOr => Ok(Self::Or),
            tacky::BinaryOperator::BitwiseXor => Ok(Self::Xor),
            tacky::BinaryOperator::LeftShift => Ok(Self::Shl),
            tacky::BinaryOperator::RightShift => Ok(Self::Sar),
            _ => Err(format!("Unsupported assembly operator: {:?}", op)),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "addl"),
            Self::Sub => write!(f, "subl"),
            Self::Mul => write!(f, "imull"),
            Self::And => write!(f, "andl"),
            Self::Or => write!(f, "orl"),
            Self::Xor => write!(f, "xorl"),
            Self::Shl => write!(f, "shll"),
            Self::Sar => write!(f, "sarl"),
        }
    }
}

/// Represents conditional comparison outcomes used for branching.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Condition {
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EqualEqual => write!(f, "e"),
            Self::NotEqual => write!(f, "ne"),
            Self::LessThan => write!(f, "l"),
            Self::GreaterThan => write!(f, "g"),
            Self::LessThanOrEq => write!(f, "le"),
            Self::GreaterThanOrEq => write!(f, "ge"),
        }
    }
}

impl TryFrom<tacky::BinaryOperator> for Condition {
    type Error = String;

    fn try_from(op: tacky::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            tacky::BinaryOperator::EqualEqual => Ok(Self::EqualEqual),
            tacky::BinaryOperator::NotEqual => Ok(Self::NotEqual),
            tacky::BinaryOperator::LessThan => Ok(Self::LessThan),
            tacky::BinaryOperator::GreaterThan => Ok(Self::GreaterThan),
            tacky::BinaryOperator::LessThanOrEq => Ok(Self::LessThanOrEq),
            tacky::BinaryOperator::GreaterThanOrEq => Ok(Self::GreaterThanOrEq),
            _ => Err(format!("Unsupported condition operator: {:?}", op)),
        }
    }
}

// Instructions in x86-64 assembly.
#[derive(Debug, Clone, PartialEq)]
enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Cmp(Operand, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(String),
    JmpCc(Condition, String),
    SetCc(Condition, Operand),
    Label(String),
    Allocate(u64),
    Ret,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov(src, dest) => write!(f, "\tmovl {src}, {dest}"),
            Self::Allocate(size) => write!(f, "\tsubq ${size}, %rsp"),
            Self::Unary(op, oprd) => write!(f, "\t{op} {oprd}"),
            Self::Ret => write!(f, "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Cdq => write!(f, "\tcdq"),
            Self::Idiv(oprd) => write!(f, "\tidivl {oprd}"),
            Self::Binary(op, lhs, rhs) => match op {
                BinaryOperator::Shl | BinaryOperator::Sar => match (lhs, rhs) {
                    (Operand::Reg(r1), Operand::Reg(r2)) => {
                        write!(f, "\t{} {}, {}", op, r1.as_byte(), r2.as_byte())
                    }
                    (Operand::Reg(r), _) => write!(f, "\t{} {}, {}", op, r.as_byte(), rhs),
                    (_, Operand::Reg(r)) => write!(f, "\t{} {}, {}", op, lhs, r.as_byte()),
                    _ => write!(f, "\t{} {}, {}", op, lhs, rhs),
                },
                _ => write!(f, "\t{} {}, {}", op, lhs, rhs),
            },
            Self::Cmp(lhs, rhs) => write!(f, "\tcmpl {}, {}", lhs, rhs),
            Self::Jmp(label) => write!(f, "\tjmp _L.{}", label),
            Self::JmpCc(cond, label) => write!(f, "\tj{} _L.{}", cond, label),
            Self::SetCc(cond, oprd) => match oprd {
                Operand::Reg(r) => write!(f, "\tset{} {}", cond, r.as_byte()),
                _ => write!(f, "\tset{} {}", cond, oprd),
            },
            Self::Label(label) => write!(f, "_L.{}:", label),
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
fn generate_pseudo_instructions(
    instruction: tacky::Instruction,
) -> Result<Vec<Instruction>, String> {
    let mut instructions: Vec<Instruction> = Vec::new();
    match instruction {
        tacky::Instruction::Return(operand) => {
            let op = Operand::from(operand);
            instructions.push(Instruction::Mov(op, Operand::Reg(Register::AX)));
            instructions.push(Instruction::Ret);
        }
        tacky::Instruction::Unary(tacky::UnaryOperator::LogicalNot, src, dst) => {
            let dst = Operand::from(dst);
            instructions.push(Instruction::Cmp(Operand::Imm(0), Operand::from(src)));
            instructions.push(Instruction::Mov(Operand::Imm(0), dst.clone()));
            instructions.push(Instruction::SetCc(Condition::EqualEqual, dst));
        }
        tacky::Instruction::Unary(op, src, dest) => {
            let op = UnaryOperator::try_from(op)?;
            let src = Operand::from(src);
            let dest = Operand::from(dest);
            instructions.push(Instruction::Mov(src, dest.clone()));
            instructions.push(Instruction::Unary(op, dest));
        }
        tacky::Instruction::Binary(op, lhs, rhs, dest) => match op {
            tacky::BinaryOperator::Divide => {
                let lhs = Operand::from(lhs);
                let rhs = Operand::from(rhs);
                let dest = Operand::from(dest);
                instructions.push(Instruction::Mov(lhs, Operand::Reg(Register::AX)));
                instructions.push(Instruction::Cdq);
                instructions.push(Instruction::Idiv(rhs));
                instructions.push(Instruction::Mov(Operand::Reg(Register::AX), dest));
            }
            tacky::BinaryOperator::Remainder => {
                let lhs = Operand::from(lhs);
                let rhs = Operand::from(rhs);
                let dest = Operand::from(dest);
                instructions.push(Instruction::Mov(lhs, Operand::Reg(Register::AX)));
                instructions.push(Instruction::Cdq);
                instructions.push(Instruction::Idiv(rhs));
                instructions.push(Instruction::Mov(Operand::Reg(Register::DX), dest));
            }
            tacky::BinaryOperator::RightShift | tacky::BinaryOperator::LeftShift => {
                let lhs = Operand::from(lhs);
                let rhs = Operand::from(rhs);
                let dest = Operand::from(dest);
                let op = BinaryOperator::try_from(op).unwrap();
                instructions.push(Instruction::Mov(lhs, dest.clone()));
                instructions.push(Instruction::Mov(rhs, Operand::Reg(Register::CX)));
                instructions.push(Instruction::Binary(op, Operand::Reg(Register::CX), dest));
            }
            tacky::BinaryOperator::EqualEqual
            | tacky::BinaryOperator::NotEqual
            | tacky::BinaryOperator::LessThan
            | tacky::BinaryOperator::GreaterThan
            | tacky::BinaryOperator::LessThanOrEq
            | tacky::BinaryOperator::GreaterThanOrEq => {
                let dst = Operand::from(dest);
                let op = Condition::try_from(op).unwrap();
                instructions.push(Instruction::Cmp(Operand::from(rhs), Operand::from(lhs)));
                instructions.push(Instruction::Mov(Operand::Imm(0), dst.clone()));
                instructions.push(Instruction::SetCc(op, dst));
            }
            _ => {
                let dst = Operand::from(dest);
                let op = BinaryOperator::try_from(op).unwrap();
                instructions.push(Instruction::Mov(Operand::from(lhs), dst.clone()));
                instructions.push(Instruction::Binary(op, Operand::from(rhs), dst));
            }
        },
        tacky::Instruction::JumpIfZero(val, tar) => {
            instructions.push(Instruction::Cmp(Operand::Imm(0), Operand::from(val)));
            instructions.push(Instruction::JmpCc(Condition::EqualEqual, tar));
        }
        tacky::Instruction::JumpIfNotZero(val, tar) => {
            instructions.push(Instruction::Cmp(Operand::Imm(0), Operand::from(val)));
            instructions.push(Instruction::JmpCc(Condition::NotEqual, tar));
        }
        tacky::Instruction::Jump(label) => {
            instructions.push(Instruction::Jmp(label));
        }
        tacky::Instruction::Label(label) => {
            instructions.push(Instruction::Label(label));
        }
        tacky::Instruction::Copy(lhs, rhs) => {
            instructions.push(Instruction::Mov(Operand::from(lhs), Operand::from(rhs)));
        }
    };
    Ok(instructions)
}

// Replace pseudo-operands with stack offsets.
fn replace_pseudo_operands(instruction: Instruction, offsets: &mut StackOffsets) -> Instruction {
    match instruction {
        Instruction::Mov(Operand::Pseudo(n1), Operand::Pseudo(n2)) => {
            let offset1 = offsets.get(&n1);
            let offset2 = offsets.get(&n2);
            Instruction::Mov(Operand::Stack(offset1), Operand::Stack(offset2))
        }
        Instruction::Mov(op, Operand::Pseudo(n)) => {
            Instruction::Mov(op, Operand::Stack(offsets.get(&n)))
        }
        Instruction::Mov(Operand::Pseudo(n), op) => {
            Instruction::Mov(Operand::Stack(offsets.get(&n)), op)
        }
        Instruction::Unary(op, Operand::Pseudo(name)) => {
            Instruction::Unary(op, Operand::Stack(offsets.get(&name)))
        }
        Instruction::Binary(op, Operand::Pseudo(n1), Operand::Pseudo(n2)) => Instruction::Binary(
            op,
            Operand::Stack(offsets.get(&n1)),
            Operand::Stack(offsets.get(&n2)),
        ),
        Instruction::Binary(op, lhs, Operand::Pseudo(name)) => {
            Instruction::Binary(op, lhs, Operand::Stack(offsets.get(&name)))
        }
        Instruction::Binary(op, Operand::Pseudo(name), rhs) => {
            Instruction::Binary(op, Operand::Stack(offsets.get(&name)), rhs)
        }
        Instruction::Idiv(Operand::Pseudo(name)) => {
            Instruction::Idiv(Operand::Stack(offsets.get(&name)))
        }
        Instruction::Cmp(Operand::Pseudo(n1), Operand::Pseudo(n2)) => Instruction::Cmp(
            Operand::Stack(offsets.get(&n1)),
            Operand::Stack(offsets.get(&n2)),
        ),
        Instruction::Cmp(op1, Operand::Pseudo(name)) => {
            Instruction::Cmp(op1, Operand::Stack(offsets.get(&name)))
        }
        Instruction::Cmp(Operand::Pseudo(name), op2) => {
            Instruction::Cmp(Operand::Stack(offsets.get(&name)), op2)
        }
        Instruction::SetCc(cond, Operand::Pseudo(name)) => {
            Instruction::SetCc(cond, Operand::Stack(offsets.get(&name)))
        }
        _ => instruction,
    }
}

// Fix invalid instructions that may arise during translation.
fn fix_invalid_instruction(instruction: Instruction) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    match instruction {
        Instruction::Mov(Operand::Stack(n1), Operand::Stack(n2)) => {
            instructions.push(Instruction::Mov(
                Operand::Stack(n1),
                Operand::Reg(Register::R10),
            ));
            instructions.push(Instruction::Mov(
                Operand::Reg(Register::R10),
                Operand::Stack(n2),
            ));
        }
        Instruction::Idiv(Operand::Imm(v)) => {
            instructions.push(Instruction::Mov(
                Operand::Imm(v),
                Operand::Reg(Register::R10),
            ));
            instructions.push(Instruction::Idiv(Operand::Reg(Register::R10)));
        }
        Instruction::Binary(BinaryOperator::Mul, lhs, Operand::Stack(n)) => {
            instructions.push(Instruction::Mov(
                Operand::Stack(n),
                Operand::Reg(Register::R11),
            ));
            instructions.push(Instruction::Binary(
                BinaryOperator::Mul,
                lhs,
                Operand::Reg(Register::R11),
            ));
            instructions.push(Instruction::Mov(
                Operand::Reg(Register::R11),
                Operand::Stack(n),
            ));
        }
        Instruction::Cmp(Operand::Stack(n1), Operand::Stack(n2)) => {
            instructions.push(Instruction::Mov(
                Operand::Stack(n1),
                Operand::Reg(Register::R10),
            ));
            instructions.push(Instruction::Cmp(
                Operand::Reg(Register::R10),
                Operand::Stack(n2),
            ));
        }
        Instruction::Cmp(lhs, Operand::Imm(val)) => {
            instructions.push(Instruction::Mov(
                Operand::Imm(val),
                Operand::Reg(Register::R11),
            ));
            instructions.push(Instruction::Cmp(lhs, Operand::Reg(Register::R11)));
        }
        Instruction::Binary(op, Operand::Stack(n1), Operand::Stack(n2)) => {
            instructions.push(Instruction::Mov(
                Operand::Stack(n1),
                Operand::Reg(Register::R10),
            ));
            instructions.push(Instruction::Binary(
                op,
                Operand::Reg(Register::R10),
                Operand::Stack(n2),
            ));
        }
        _ => instructions.push(instruction),
    };
    instructions
}

impl TryFrom<tacky::Function> for Function {
    type Error = String;

    fn try_from(func: tacky::Function) -> Result<Self, String> {
        let name = String::from(func.name());

        let mut instructions: Vec<Instruction> = Vec::new();
        instructions.push(Instruction::Allocate(0));

        for instr in Vec::from(func) {
            let asm_instructions = generate_pseudo_instructions(instr)?;
            instructions.extend(asm_instructions);
        }

        let mut offsets = StackOffsets::default();
        let mut instructions = instructions
            .into_iter()
            .map(|instr| replace_pseudo_operands(instr, &mut offsets))
            .collect::<Vec<Instruction>>();

        let size = offsets.size();
        instructions[0] = Instruction::Allocate(size);

        let instructions = instructions
            .into_iter()
            .flat_map(|instr| fix_invalid_instruction(instr))
            .collect::<Vec<Instruction>>();

        Ok(Self(name, instructions))
    }
}

// Represents a program in x86-64 assembly.
#[derive(Debug, Clone, PartialEq)]
pub struct Program(Function);

impl TryFrom<tacky::Program> for Program {
    type Error = String;

    fn try_from(program: tacky::Program) -> Result<Self, String> {
        let function = tacky::Function::from(program);
        Ok(Self(Function::try_from(function)?))
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
