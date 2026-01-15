use std::fmt;

use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;

use super::parser;

/// Represents unary operators that operate on a single operand.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negation,
    LogicalNot,
    Decrement,
    Increment,
}

impl From<parser::UnaryOperator> for UnaryOperator {
    fn from(op: parser::UnaryOperator) -> Self {
        match op {
            parser::UnaryOperator::Complement => Self::Complement,
            parser::UnaryOperator::Negation => Self::Negation,
            parser::UnaryOperator::LogicalNot => Self::LogicalNot,
            parser::UnaryOperator::Decrement => Self::Decrement,
            parser::UnaryOperator::Increment => Self::Increment,
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Complement => write!(f, "~"),
            Self::Negation => write!(f, "-"),
            Self::LogicalNot => write!(f, "!"),
            Self::Decrement { .. } => write!(f, "--"),
            Self::Increment { .. } => write!(f, "++"),
        }
    }
}

/// Represents binary operators that operate on two operands.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
}

impl TryFrom<parser::BinaryOperator> for BinaryOperator {
    type Error = String;

    fn try_from(op: parser::BinaryOperator) -> Result<Self, String> {
        match op {
            parser::BinaryOperator::Add => Ok(Self::Add),
            parser::BinaryOperator::Substract => Ok(Self::Subtract),
            parser::BinaryOperator::Multiply => Ok(Self::Multiply),
            parser::BinaryOperator::Divide => Ok(Self::Divide),
            parser::BinaryOperator::Remainder => Ok(Self::Remainder),
            parser::BinaryOperator::BitwiseAnd => Ok(Self::BitwiseAnd),
            parser::BinaryOperator::BitwiseOr => Ok(Self::BitwiseOr),
            parser::BinaryOperator::BitwiseXor => Ok(Self::BitwiseXor),
            parser::BinaryOperator::LeftShift => Ok(Self::LeftShift),
            parser::BinaryOperator::RightShift => Ok(Self::RightShift),
            parser::BinaryOperator::EqualEqual => Ok(Self::EqualEqual),
            parser::BinaryOperator::NotEqual => Ok(Self::NotEqual),
            parser::BinaryOperator::LessThan => Ok(Self::LessThan),
            parser::BinaryOperator::GreaterThan => Ok(Self::GreaterThan),
            parser::BinaryOperator::LessThanOrEq => Ok(Self::LessThanOrEq),
            parser::BinaryOperator::GreaterThanOrEq => Ok(Self::GreaterThanOrEq),
            parser::BinaryOperator::AddAssignment => Ok(Self::Add),
            parser::BinaryOperator::SubAssignment => Ok(Self::Subtract),
            parser::BinaryOperator::DivAssignment => Ok(Self::Divide),
            parser::BinaryOperator::RemAssignment => Ok(Self::Remainder),
            parser::BinaryOperator::ProdAssignment => Ok(Self::Multiply),
            parser::BinaryOperator::AndAssignment => Ok(Self::BitwiseAnd),
            parser::BinaryOperator::OrAssignment => Ok(Self::BitwiseOr),
            parser::BinaryOperator::XorAssignment => Ok(Self::BitwiseXor),
            parser::BinaryOperator::LShiftAssignment => Ok(Self::LeftShift),
            parser::BinaryOperator::RShiftAssignment => Ok(Self::RightShift),
            _ => Err(format!("Operator '{:?}' is not a tacky binary op.", op)),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Remainder => write!(f, "%"),
            Self::BitwiseAnd => write!(f, "&"),
            Self::BitwiseOr => write!(f, "|"),
            Self::BitwiseXor => write!(f, "^"),
            Self::LeftShift => write!(f, "<<"),
            Self::RightShift => write!(f, ">>"),
            Self::EqualEqual => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThanOrEq => write!(f, "<="),
            Self::GreaterThanOrEq => write!(f, ">="),
        }
    }
}

/// Represents an operand in an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Constant(u64),
    Variable(String),
}

impl Operand {
    fn temp() -> Self {
        Self::Variable(nanoid!(21, ALPHANUMERIC))
    }
}

impl TryFrom<parser::Factor> for Operand {
    type Error = String;

    fn try_from(value: parser::Factor) -> Result<Self, String> {
        match value {
            parser::Factor::Expression(expression) => Self::try_from(*expression),
            parser::Factor::Identifier(name) => Ok(Self::Variable(name)),
            parser::Factor::Int(integer) => Ok(Self::Constant(integer)),
            _ => Err(String::from("Factor is not an operand")),
        }
    }
}

impl TryFrom<parser::Expression> for Operand {
    type Error = String;

    fn try_from(value: parser::Expression) -> Result<Self, String> {
        if let parser::Expression::Factor(factor) = value {
            Self::try_from(factor)
        } else {
            Err(String::from("Expression is not an operand"))
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant(c) => write!(f, "{}", c),
            Self::Variable(v) => write!(f, "{}", v),
        }
    }
}

/// Represents a low-level instruction in the intermediate representation.
pub enum Instruction {
    Return(Operand),
    Unary(UnaryOperator, Operand, Operand),
    Binary(BinaryOperator, Operand, Operand, Operand),
    Copy(Operand, Operand),
    Jump(String),
    JumpIfZero(Operand, String),
    JumpIfNotZero(Operand, String),
    Label(String),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "Return {val}"),
            Instruction::Unary(op, src, dst) => write!(f, "{op} {src} {dst}"),
            Instruction::Binary(op, lhs, rhs, dst) => write!(f, "{op} {lhs} {rhs} {dst}"),
            Instruction::Copy(src, dst) => write!(f, "Copy {src} {dst}"),
            Instruction::Jump(lb) => write!(f, "Jump {lb}"),
            Instruction::JumpIfZero(cond, lb) => write!(f, "Jump If Zero {cond} {lb}"),
            Instruction::JumpIfNotZero(cond, lb) => write!(f, "Jump If Not Zero {cond} {lb}"),
            Instruction::Label(lb) => write!(f, "Label {lb}"),
        }
    }
}

/// Linearizes the given assignment into TAC
fn lin_assign_expr(
    lhs: parser::Expression,
    rhs: parser::Expression,
    op: parser::BinaryOperator,
    body: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    let rhs = lin_expr(rhs, body)?;
    let lhs = Operand::try_from(lhs)?;

    if !matches!(op, parser::BinaryOperator::Assignment) {
        let temp = Operand::temp();
        let op = BinaryOperator::try_from(op)
            .map_err(|_| format!("Expected compound asssignment op, found: {op}"))?;

        body.push(Instruction::Binary(op, lhs.clone(), rhs, temp.clone()));
        body.push(Instruction::Copy(temp, lhs.clone()));
    } else {
        body.push(Instruction::Copy(rhs, lhs.clone()));
    };
    Ok(lhs)
}

/// Linearizes the logical expression into TAC
fn lin_log_expr(
    lhs: parser::Expression,
    rhs: parser::Expression,
    op: parser::BinaryOperator,
    body: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    let lhs = lin_expr(lhs, body)?;

    let mut instructions: Vec<Instruction> = Vec::new();
    let rhs = lin_expr(rhs, &mut instructions)?;

    let dst = Operand::temp();
    let end = format!("label.{}", nanoid!(21, ALPHANUMERIC));

    if matches!(op, parser::BinaryOperator::LogicalAnd) {
        let isfalse = format!("label.{}", nanoid!(21, ALPHANUMERIC));
        body.push(Instruction::JumpIfZero(lhs, String::from(&isfalse)));
        body.extend(instructions);
        body.push(Instruction::JumpIfZero(rhs, String::from(&isfalse)));
        body.push(Instruction::Copy(Operand::Constant(1), dst.clone()));
        body.push(Instruction::Jump(end.clone()));
        body.push(Instruction::Label(isfalse));
        body.push(Instruction::Copy(Operand::Constant(0), dst.clone()));
        body.push(Instruction::Label(end));
    } else {
        let istrue = format!("label.{}", nanoid!(21, ALPHANUMERIC));
        body.push(Instruction::JumpIfNotZero(lhs, String::from(&istrue)));
        body.extend(instructions);
        body.push(Instruction::JumpIfNotZero(rhs, String::from(&istrue)));
        body.push(Instruction::Copy(Operand::Constant(0), dst.clone()));
        body.push(Instruction::Jump(end.clone()));
        body.push(Instruction::Label(istrue));
        body.push(Instruction::Copy(Operand::Constant(1), dst.clone()));
        body.push(Instruction::Label(end));
    }
    Ok(dst)
}

/// Linearizes the given binary expression into TAC
fn lin_bin_expr(
    op: parser::BinaryOperator,
    lhs: parser::Expression,
    rhs: parser::Expression,
    body: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    match op {
        parser::BinaryOperator::LogicalAnd => lin_log_expr(lhs, rhs, op, body),
        parser::BinaryOperator::LogicalOr => lin_log_expr(lhs, rhs, op, body),
        parser::BinaryOperator::Assignment => lin_assign_expr(lhs, rhs, op, body),
        op if op.is_assignment() => lin_assign_expr(lhs, rhs, op, body),
        op => {
            let dst = Operand::temp();
            let lhs = lin_expr(lhs, body)?;
            let rhs = lin_expr(rhs, body)?;
            let op = BinaryOperator::try_from(op).unwrap();
            body.push(Instruction::Binary(op, lhs, rhs, dst.clone()));
            Ok(dst)
        }
    }
}

/// Linearizes the given factor into TAC
fn lin_factor(factor: parser::Factor, body: &mut Vec<Instruction>) -> Result<Operand, String> {
    match factor {
        parser::Factor::Int(n) => Ok(Operand::Constant(n)),
        parser::Factor::Expression(expression) => Ok(lin_expr(*expression, body)?),
        parser::Factor::Identifier(identifier) => Ok(Operand::Variable(identifier)),
        parser::Factor::Unary(op, fixity, fac) => {
            let dst = Operand::temp();
            match op {
                parser::UnaryOperator::Increment | parser::UnaryOperator::Decrement => {
                    let diff = Operand::Constant(1);
                    let op = if matches!(op, parser::UnaryOperator::Increment) {
                        BinaryOperator::Add
                    } else {
                        BinaryOperator::Subtract
                    };
                    if let parser::Fixity::Prefix = fixity {
                        let src = lin_factor(*fac, body)?;
                        body.push(Instruction::Binary(op, src.clone(), diff, dst.clone()));
                        body.push(Instruction::Copy(dst.clone(), src));
                        Ok(dst)
                    } else {
                        let src = lin_factor(*fac, body)?;
                        body.push(Instruction::Copy(src.clone(), dst.clone()));
                        let temp = Operand::temp();
                        body.push(Instruction::Binary(op, src.clone(), diff, temp.clone()));
                        body.push(Instruction::Copy(temp.clone(), src.clone()));
                        Ok(dst)
                    }
                }
                _ => {
                    let src = lin_factor(*fac, body)?;
                    let dst = Operand::temp();
                    let op = UnaryOperator::from(op);
                    body.push(Instruction::Unary(op, src, dst.clone()));
                    Ok(dst)
                }
            }
        }
    }
}

/// Linearizes the given expression into TAC
fn lin_expr(
    expression: parser::Expression,
    body: &mut Vec<Instruction>,
) -> Result<Operand, String> {
    match expression {
        parser::Expression::Binary(lhs, op, rhs) => lin_bin_expr(op, *lhs, *rhs, body),
        parser::Expression::Factor(factor) => lin_factor(factor, body),
        parser::Expression::Conditional(cond, then, otherwise) => {
            let cond = lin_expr(*cond, body)?;
            let dst = Operand::temp();
            let alt = format!("label.{}", nanoid!(21, ALPHANUMERIC));
            let end = format!("label.{}", nanoid!(21, ALPHANUMERIC));
            body.push(Instruction::JumpIfZero(cond, alt.clone()));
            let then = lin_expr(*then, body)?;
            body.push(Instruction::Copy(then, dst.clone()));
            body.push(Instruction::Jump(end.clone()));
            body.push(Instruction::Label(alt));
            let otherwise = lin_expr(*otherwise, body)?;
            body.push(Instruction::Copy(otherwise, dst.clone()));
            body.push(Instruction::Label(end));
            Ok(dst)
        }
    }
}

fn lin_stmt(statement: parser::Statement, body: &mut Vec<Instruction>) -> Result<(), String> {
    match statement {
        parser::Statement::Expression(expr) => {
            let _ = lin_expr(expr, body)?;
            Ok(())
        }
        parser::Statement::Return(expr) => {
            let res = lin_expr(expr, body)?;
            body.push(Instruction::Return(res.clone()));
            Ok(())
        }
        parser::Statement::Null => Ok(()),
        parser::Statement::If(cond, then, otherwise) => {
            let cond = lin_expr(cond, body)?;
            let end = format!("label.{}", nanoid!(21, ALPHANUMERIC));
            if otherwise.is_none() {
                body.push(Instruction::JumpIfZero(cond, end.clone()));
                let _ = lin_stmt(*then, body)?;
            } else {
                let alt = format!("label.{}", nanoid!(21, ALPHANUMERIC));
                body.push(Instruction::JumpIfZero(cond, alt.clone()));
                let _ = lin_stmt(*then, body);
                body.push(Instruction::Jump(end.clone()));
                body.push(Instruction::Label(alt));
                let _ = lin_stmt(*otherwise.unwrap(), body);
            }
            body.push(Instruction::Label(end));
            Ok(())
        }
        parser::Statement::Goto(label) => {
            body.push(Instruction::Jump(label));
            Ok(())
        }
        parser::Statement::Label(label, stmt) => {
            body.push(Instruction::Label(label));
            lin_stmt(*stmt, body)
        }
        parser::Statement::Compound(block) => {
            for item in block.items() {
                lin_block_item(item, body)?;
            }
            Ok(())
        }
        parser::Statement::Break(label) => {
            body.push(Instruction::Jump(format!("label.break.{label}")));
            Ok(())
        }
        parser::Statement::Continue(label) => {
            body.push(Instruction::Label(format!("label.continue.{label}")));
            Ok(())
        }
        parser::Statement::DoWhile(bd, cond, label) => {
            let start = format!("label.{}", nanoid!(21, ALPHANUMERIC));
            let continue_label = format!("label.continue.{}", label);
            body.push(Instruction::Label(start.clone()));
            lin_stmt(*bd, body)?;
            body.push(Instruction::Label(continue_label));
            let op = lin_expr(cond, body)?;
            body.push(Instruction::JumpIfNotZero(op, start));
            let break_label = format!("label.break.{}", label);
            body.push(Instruction::Label(break_label));
            Ok(())
        }
        parser::Statement::While(cond, bd, label) => {
            let continue_label = format!("label.continue.{}", label);
            let break_label = format!("label.break.{}", label);
            body.push(Instruction::Label(continue_label.clone()));
            let op = lin_expr(cond, body)?;
            body.push(Instruction::JumpIfZero(op, break_label.clone()));
            lin_stmt(*bd, body)?;
            body.push(Instruction::Jump(continue_label));
            body.push(Instruction::Label(break_label));
            Ok(())
        }
        parser::Statement::For(init, cond, post, bd, label) => {
            let break_label = format!("label.break.{label}");
            let start = format!("label.{}", nanoid!(21, ALPHANUMERIC));
            if let Some(init) = init {
                match init {
                    parser::ForInit::InitDecl(decl) => lin_decl(decl, body),
                    parser::ForInit::InitExpr(expr) => {
                        let _ = lin_expr(expr, body)?;
                        Ok(())
                    }
                }?;
            };
            body.push(Instruction::Label(start.clone()));
            if let Some(cond) = cond {
                let op = lin_expr(cond, body)?;
                body.push(Instruction::JumpIfZero(op, break_label.clone()));
            }
            lin_stmt(*bd, body)?;
            body.push(Instruction::Label(format!("label.continue.{label}")));
            if let Some(post) = post {
                let _ = lin_expr(post, body)?;
            }
            body.push(Instruction::Jump(start));
            body.push(Instruction::Label(break_label));
            Ok(())
        }
    }
}

fn lin_decl(decl: parser::Declaration, body: &mut Vec<Instruction>) -> Result<(), String> {
    let name = String::from(decl.name());
    if let Some(expr) = decl.initializer() {
        let opr = lin_expr(expr, body)?;
        let dst = Operand::Variable(name);
        body.push(Instruction::Copy(opr, dst));
    };
    Ok(())
}

fn lin_block_item(item: parser::BlockItem, body: &mut Vec<Instruction>) -> Result<(), String> {
    match item {
        parser::BlockItem::Declaration(decl) => lin_decl(decl, body),
        parser::BlockItem::Statement(stmt) => lin_stmt(stmt, body),
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

impl TryFrom<parser::Function> for Function {
    type Error = String;

    fn try_from(function: parser::Function) -> Result<Self, String> {
        let name = String::from(function.name());
        let mut body: Vec<Instruction> = Vec::new();

        for item in function.instructions() {
            match item {
                parser::BlockItem::Declaration(decl) => {
                    let name = String::from(decl.name());
                    if let Some(expr) = decl.initializer() {
                        let opr = lin_expr(expr, &mut body)?;
                        let dst = Operand::Variable(name);
                        body.push(Instruction::Copy(opr, dst));
                    }
                }
                parser::BlockItem::Statement(stmt) => lin_stmt(stmt, &mut body)?,
            };
        }
        match body.last() {
            Some(Instruction::Return(_)) => (),
            _ => body.push(Instruction::Return(Operand::Constant(0))),
        };
        Ok(Function(name, body))
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

impl TryFrom<parser::Program> for Program {
    type Error = String;

    fn try_from(program: parser::Program) -> Result<Self, String> {
        let function = parser::Function::from(program);
        Ok(Program(Function::try_from(function)?))
    }
}
