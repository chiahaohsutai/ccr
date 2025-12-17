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
