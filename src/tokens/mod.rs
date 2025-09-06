pub mod delimiter;
pub mod keyword;

pub use delimiter::Delimiter;
pub use keyword::Keyword;

/// Tokens in C source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    IDENTIFIER(String),
    CONSTANT(i64),
    KEYWORD(Keyword),
    DELIMITER(Delimiter),
}
