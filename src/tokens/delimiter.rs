use std::process;

/// Delimiters in C source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    LPAREN,
    RPAREN,
    SEMICOLON,
    LBRACE,
    RBRACE,
}

impl From<&str> for Delimiter {
    fn from(s: &str) -> Self {
        match s {
            "(" => Delimiter::LPAREN,
            ")" => Delimiter::RPAREN,
            ";" => Delimiter::SEMICOLON,
            "{" => Delimiter::LBRACE,
            "}" => Delimiter::RBRACE,
            _ => {
                eprintln!("Unknown delimiter: {}", s);
                process::exit(1);
            },
        }
    }
}
