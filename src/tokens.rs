use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    DECREMENT,
    NEGATION,
    COMPLEMENT,
    ADDITION,
    PRODUCT,
    DIVISION,
    REMAINDER,
    BITWISEAND,
    BITWISEOR,
    BITWISEXOR,
    LEFTSHIFT,
    RIGHTSHIFT,
    NOT,
    AND,
    OR,
    EQUAL,
    NOTEQUAL,
    LESSTHAN,
    GREATERTHAN,
    LESSEQUAL,
    GREATEREQUAL,
}

impl FromStr for Operator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "--" => Ok(Operator::DECREMENT),
            "-" => Ok(Operator::NEGATION),
            "~" => Ok(Operator::COMPLEMENT),
            "+" => Ok(Operator::ADDITION),
            "*" => Ok(Operator::PRODUCT),
            "/" => Ok(Operator::DIVISION),
            "%" => Ok(Operator::REMAINDER),
            "&" => Ok(Operator::BITWISEAND),
            "|" => Ok(Operator::BITWISEOR),
            "^" => Ok(Operator::BITWISEXOR),
            "<<" => Ok(Operator::LEFTSHIFT),
            ">>" => Ok(Operator::RIGHTSHIFT),
            "!" => Ok(Operator::NOT),
            "&&" => Ok(Operator::AND),
            "||" => Ok(Operator::OR),
            "==" => Ok(Operator::EQUAL),
            "!=" => Ok(Operator::NOTEQUAL),
            "<" => Ok(Operator::LESSTHAN),
            ">" => Ok(Operator::GREATERTHAN),
            "<=" => Ok(Operator::LESSEQUAL),
            ">=" => Ok(Operator::GREATEREQUAL),
            _ => Err(format!("Unknown operator: {}", s)),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::DECREMENT => write!(f, "--"),
            Operator::NEGATION => write!(f, "-"),
            Operator::COMPLEMENT => write!(f, "~"),
            Operator::ADDITION => write!(f, "+"),
            Operator::PRODUCT => write!(f, "*"),
            Operator::DIVISION => write!(f, "/"),
            Operator::REMAINDER => write!(f, "%"),
            Operator::BITWISEAND => write!(f, "&"),
            Operator::BITWISEOR => write!(f, "|"),
            Operator::BITWISEXOR => write!(f, "^"),
            Operator::LEFTSHIFT => write!(f, "<<"),
            Operator::RIGHTSHIFT => write!(f, ">>"),
            Operator::NOT => write!(f, "!"),
            Operator::AND => write!(f, "&&"),
            Operator::OR => write!(f, "||"),
            Operator::EQUAL => write!(f, "=="),
            Operator::NOTEQUAL => write!(f, "!="),
            Operator::LESSTHAN => write!(f, "<"),
            Operator::GREATERTHAN => write!(f, ">"),
            Operator::LESSEQUAL => write!(f, "<="),
            Operator::GREATEREQUAL => write!(f, ">="),
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
    CONSTANT(u64),
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

impl Token {
    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            Token::OPERATOR(Operator::ADDITION)
                | Token::OPERATOR(Operator::PRODUCT)
                | Token::OPERATOR(Operator::DIVISION)
                | Token::OPERATOR(Operator::REMAINDER)
                | Token::OPERATOR(Operator::NEGATION)
                | Token::OPERATOR(Operator::BITWISEAND)
                | Token::OPERATOR(Operator::BITWISEOR)
                | Token::OPERATOR(Operator::BITWISEXOR)
                | Token::OPERATOR(Operator::LEFTSHIFT)
                | Token::OPERATOR(Operator::RIGHTSHIFT)
        )
    }
    pub fn precedence(&self) -> u64 {
        match self {
            Self::OPERATOR(Operator::BITWISEOR) => 25,
            Self::OPERATOR(Operator::BITWISEXOR) => 30,
            Self::OPERATOR(Operator::BITWISEAND) => 35,
            Self::OPERATOR(Operator::RIGHTSHIFT) | Self::OPERATOR(Operator::LEFTSHIFT) => 40,
            Self::OPERATOR(Operator::ADDITION) | Self::OPERATOR(Operator::NEGATION) => 45,
            Self::OPERATOR(Operator::PRODUCT)
            | Self::OPERATOR(Operator::DIVISION)
            | Self::OPERATOR(Operator::REMAINDER) => 50,
            _ => 0,
        }
    }
}
