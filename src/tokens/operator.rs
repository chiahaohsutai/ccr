use std::process;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    DECREMENT,
    NEGATION,
    BITWISENOT,
}

impl From<&str> for UnaryOp {
    fn from(s: &str) -> Self {
        match s {
            "--" => UnaryOp::DECREMENT,
            "-" => UnaryOp::NEGATION,
            "~" => UnaryOp::BITWISENOT,
            _ => {
                eprintln!("Unknown unary operator: {}", s);
                process::exit(1);
            }
        }
    }
}

/// Operators in C source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    UNARY(UnaryOp),
}
