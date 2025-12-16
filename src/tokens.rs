use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    DECREMENT,
    NEGATION,
    BITWISENOT,
}

impl FromStr for Operator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "--" => Ok(Operator::DECREMENT),
            "-" => Ok(Operator::NEGATION),
            "~" => Ok(Operator::BITWISENOT),
            _ => Err(format!("Unknown operator: {}", s)),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::DECREMENT => write!(f, "--"),
            Operator::NEGATION => write!(f, "-"),
            Operator::BITWISENOT => write!(f, "~"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    INT,
    VOID,
    RETURN,
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Keyword::INT),
            "void" => Ok(Keyword::VOID),
            "return" => Ok(Keyword::RETURN),
            _ => Err(format!("Unknown keyword: {}", s)),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::INT => write!(f, "int"),
            Keyword::VOID => write!(f, "void"),
            Keyword::RETURN => write!(f, "return"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter {
    LPAREN,
    RPAREN,
    SEMICOLON,
    LBRACE,
    RBRACE,
}

impl FromStr for Delimiter {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(Delimiter::LPAREN),
            ")" => Ok(Delimiter::RPAREN),
            ";" => Ok(Delimiter::SEMICOLON),
            "{" => Ok(Delimiter::LBRACE),
            "}" => Ok(Delimiter::RBRACE),
            _ => Err(format!("Unknown delimiter: {}", s)),
        }
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Delimiter::LPAREN => write!(f, "("),
            Delimiter::RPAREN => write!(f, ")"),
            Delimiter::SEMICOLON => write!(f, ";"),
            Delimiter::LBRACE => write!(f, "{{"),
            Delimiter::RBRACE => write!(f, "}}"),
        }
    }
}

/// Tokens in C source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    CONSTANT(i64),
    KEYWORD(Keyword),
    IDENTIFIER(String),
    OPERATOR(Operator),
    DELIMITER(Delimiter),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::CONSTANT(value) => write!(f, "{}", value),
            Token::KEYWORD(kw) => write!(f, "{}", kw),
            Token::IDENTIFIER(name) => write!(f, "{}", name),
            Token::OPERATOR(op) => write!(f, "{}", op),
            Token::DELIMITER(delim) => write!(f, "{}", delim),
        }
    }
}
