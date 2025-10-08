use std::process;

/// Keywords in C source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    INT,
    VOID,
    RETURN,
}

impl From<&str> for Keyword {
    fn from(s: &str) -> Self {
        match s {
            "int" => Keyword::INT,
            "void" => Keyword::VOID,
            "return" => Keyword::RETURN,
            _ => {
                eprintln!("Unknown keyword: {}", s);
                process::exit(1);
            },
        }
    }
}
