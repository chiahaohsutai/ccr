mod delimiter;
mod keyword;
mod operator;

pub use delimiter::Delimiter;
pub use keyword::Keyword;
pub use operator::{Operator, UnaryOp};

/// Tokens in C source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    IDENTIFIER(String),
    CONSTANT(i64),
    KEYWORD(Keyword),
    OPERATOR(Operator),
    DELIMITER(Delimiter),
}
