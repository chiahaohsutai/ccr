use super::parser;
use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    COMPLEMENT,
    NEGATE,
    NOT,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::COMPLEMENT => write!(f, "~"),
            UnaryOperator::NEGATE => write!(f, "-"),
            UnaryOperator::NOT => write!(f, "!"),
        }
    }
}

impl From<parser::UnaryOperator> for UnaryOperator {
    fn from(op: parser::UnaryOperator) -> Self {
        match op {
            parser::UnaryOperator::COMPLEMENT => UnaryOperator::COMPLEMENT,
            parser::UnaryOperator::NEGATE => UnaryOperator::NEGATE,
            parser::UnaryOperator::NOT => UnaryOperator::NOT,
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
    EQUAL,
    NOTEQUAL,
    LESSTHAN,
    GREATERTHAN,
    LESSEQUAL,
    GREATEREQUAL,
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
            BinaryOperator::EQUAL => write!(f, "=="),
            BinaryOperator::NOTEQUAL => write!(f, "!="),
            BinaryOperator::LESSTHAN => write!(f, "<"),
            BinaryOperator::GREATERTHAN => write!(f, ">"),
            BinaryOperator::LESSEQUAL => write!(f, "<="),
            BinaryOperator::GREATEREQUAL => write!(f, ">="),
        }
    }
}

impl TryFrom<parser::BinaryOperator> for BinaryOperator {
    type Error = String;

    fn try_from(op: parser::BinaryOperator) -> Result<Self, String> {
        match op {
            parser::BinaryOperator::ADD => Ok(BinaryOperator::ADD),
            parser::BinaryOperator::SUBTRACT => Ok(BinaryOperator::SUBTRACT),
            parser::BinaryOperator::MULTIPLY => Ok(BinaryOperator::MULTIPLY),
            parser::BinaryOperator::DIVIDE => Ok(BinaryOperator::DIVIDE),
            parser::BinaryOperator::REMAINDER => Ok(BinaryOperator::REMAINDER),
            parser::BinaryOperator::BITWISEAND => Ok(BinaryOperator::BITWISEAND),
            parser::BinaryOperator::BITWISEOR => Ok(BinaryOperator::BITWISEOR),
            parser::BinaryOperator::BITWISEXOR => Ok(BinaryOperator::BITWISEXOR),
            parser::BinaryOperator::LEFTSHIFT => Ok(BinaryOperator::LEFTSHIFT),
            parser::BinaryOperator::RIGHTSHIFT => Ok(BinaryOperator::RIGHTSHIFT),
            parser::BinaryOperator::EQUAL => Ok(BinaryOperator::EQUAL),
            parser::BinaryOperator::NOTEQUAL => Ok(BinaryOperator::NOTEQUAL),
            parser::BinaryOperator::LESSTHAN => Ok(BinaryOperator::LESSTHAN),
            parser::BinaryOperator::GREATERTHAN => Ok(BinaryOperator::GREATERTHAN),
            parser::BinaryOperator::LESSEQUAL => Ok(BinaryOperator::LESSEQUAL),
            parser::BinaryOperator::GREATEREQUAL => Ok(BinaryOperator::GREATEREQUAL),
            _ => Err(format!("Operator '{:?}' is not a tacky binary op.", op)),
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
    COPY(Operand, Operand),
    JUMP(String),
    JUMPIF(Operand, String),
    JUMPIFNOT(Operand, String),
    LABEL(String),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::RETURN(v) => write!(f, "RETURN {}", v),
            Instruction::UNARY(op, src, dest) => write!(f, "{} {} {}", op, src, dest),
            Instruction::BINARY(op, l, r, dest) => write!(f, "{} {} {} {}", op, l, r, dest),
            Instruction::COPY(src, dest) => write!(f, "COPY {} {}", src, dest),
            Instruction::JUMP(label) => write!(f, "JUMP {}", label),
            Instruction::JUMPIF(cond, label) => write!(f, "JUMP IF ZERO {} {}", cond, label),
            Instruction::JUMPIFNOT(cond, label) => write!(f, "JUMP IF NOT ZERO {} {}", cond, label),
            Instruction::LABEL(label) => write!(f, "LABEL {}", label),
        }
    }
}

fn generate_instructions(
    expression: parser::Expression,
    instructions: &mut Vec<Instruction>,
) -> Operand {
    match expression {
        parser::Expression::FACTOR(factor) => match factor {
            parser::Factor::INT(n) => Operand::CONSTANT(n),
            parser::Factor::UNARY(op, exp) => {
                let src = generate_instructions(parser::Expression::FACTOR(*exp), instructions);
                let dst = Operand::VARIABLE(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));
                let op = UnaryOperator::from(op);
                instructions.push(Instruction::UNARY(op, src, dst.clone()));
                dst
            }
            parser::Factor::EXPRESSION(expr) => generate_instructions(*expr, instructions),
            _ => todo!(),
        },
        parser::Expression::BINARY(lhs, op, rhs) => {
            let dst = Operand::VARIABLE(format!("temp.{}", nanoid!(21, ALPHANUMERIC)));

            if let parser::BinaryOperator::AND | parser::BinaryOperator::OR = op {
                let end = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                let lhs = generate_instructions(*lhs, instructions);

                let mut rhs_instructions: Vec<Instruction> = Vec::new();
                let rhs = generate_instructions(*rhs, &mut rhs_instructions);

                if matches!(op, parser::BinaryOperator::AND) {
                    let isfalse = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                    instructions.push(Instruction::JUMPIF(lhs, String::from(&isfalse)));
                    instructions.extend(rhs_instructions);
                    instructions.push(Instruction::JUMPIF(rhs, String::from(&isfalse)));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(1), dst.clone()));
                    instructions.push(Instruction::JUMP(end.clone()));
                    instructions.push(Instruction::LABEL(isfalse));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(0), dst.clone()));
                    instructions.push(Instruction::LABEL(end));
                } else {
                    let istrue = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                    instructions.push(Instruction::JUMPIFNOT(lhs, String::from(&istrue)));
                    instructions.extend(rhs_instructions);
                    instructions.push(Instruction::JUMPIFNOT(rhs, String::from(&istrue)));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(0), dst.clone()));
                    instructions.push(Instruction::JUMP(end.clone()));
                    instructions.push(Instruction::LABEL(istrue));
                    instructions.push(Instruction::COPY(Operand::CONSTANT(1), dst.clone()));
                    instructions.push(Instruction::LABEL(end));
                }
                dst
            } else {
                let lhs = generate_instructions(*lhs, instructions);
                let rhs = generate_instructions(*rhs, instructions);
                let op = BinaryOperator::try_from(op).unwrap();
                instructions.push(Instruction::BINARY(op, lhs, rhs, dst.clone()));
                dst
            }
        }
        _ => todo!(),
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
        let instrs = self
            .1
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<String>>()
            .join("\n\t");
        writeln!(f, "FN {}\n\t{}\nEND FN {}", self.0, instrs, self.0)
    }
}

impl From<Function> for Vec<Instruction> {
    fn from(function: Function) -> Self {
        function.1
    }
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        todo!()
        // let name = String::from(function.name().as_ref());
        // let mut instructions: Vec<Instruction> = Vec::new();

        // match parser::Statement::from(function) {
        //     parser::Statement::RETURN(expr) => {
        //         debug!("Generating instructions for return expression: {}", expr);
        //         let value = generate_instructions(expr, &mut instructions);
        //         instructions.push(Instruction::RETURN(value));
        //     }
        // };
        // Function(name, instructions)
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
