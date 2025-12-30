use regex::Regex;

use std::{cmp, fmt, str::FromStr, sync};

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
    ASSIGNMENT,
}

impl Operator {
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    fn find_match(input: &str) -> Option<Self> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^(<=|>=|--|<<|>>|&&|\|\||==|!=|[\-~\+\*\/%&\|\^!><=])";
            Regex::new(pattern).unwrap()
        });
        RE.find(input).map(|m| Self::from_str(m.as_str()).unwrap())
    }

    fn is_unary(&self) -> bool {
        match self {
            Self::DECREMENT | Self::COMPLEMENT | Self::NOT => true,
            _ => false,
        }
    }

    fn is_binary(&self) -> bool {
        !self.is_unary()
    }

    fn precedence(&self) -> u64 {
        match self {
            Self::ASSIGNMENT => 1,
            Self::OR => 5,
            Self::AND => 10,
            Self::BITWISEOR => 15,
            Self::BITWISEXOR => 20,
            Self::BITWISEAND => 25,
            Self::EQUAL | Self::NOTEQUAL => 30,
            Self::LESSTHAN | Self::GREATERTHAN | Self::LESSEQUAL | Self::GREATEREQUAL => 35,
            Self::RIGHTSHIFT | Self::LEFTSHIFT => 40,
            Self::ADDITION | Self::NEGATION => 45,
            Self::PRODUCT | Self::DIVISION | Self::REMAINDER => 50,
            _ => 0,
        }
    }
}

impl AsRef<str> for Operator {
    fn as_ref(&self) -> &str {
        match self {
            Self::DECREMENT => "--",
            Self::NEGATION => "-",
            Self::COMPLEMENT => "~",
            Self::ADDITION => "+",
            Self::PRODUCT => "*",
            Self::DIVISION => "/",
            Self::REMAINDER => "%",
            Self::BITWISEAND => "&",
            Self::BITWISEOR => "|",
            Self::BITWISEXOR => "^",
            Self::LEFTSHIFT => "<<",
            Self::RIGHTSHIFT => ">>",
            Self::NOT => "!",
            Self::AND => "&&",
            Self::OR => "||",
            Self::EQUAL => "==",
            Self::NOTEQUAL => "!=",
            Self::LESSTHAN => "<",
            Self::GREATERTHAN => ">",
            Self::LESSEQUAL => "<=",
            Self::GREATEREQUAL => ">=",
            Self::ASSIGNMENT => "=",
        }
    }
}

impl FromStr for Operator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "--" => Ok(Self::DECREMENT),
            "-" => Ok(Self::NEGATION),
            "~" => Ok(Self::COMPLEMENT),
            "+" => Ok(Self::ADDITION),
            "*" => Ok(Self::PRODUCT),
            "/" => Ok(Self::DIVISION),
            "%" => Ok(Self::REMAINDER),
            "&" => Ok(Self::BITWISEAND),
            "|" => Ok(Self::BITWISEOR),
            "^" => Ok(Self::BITWISEXOR),
            "<<" => Ok(Self::LEFTSHIFT),
            ">>" => Ok(Self::RIGHTSHIFT),
            "!" => Ok(Self::NOT),
            "&&" => Ok(Self::AND),
            "||" => Ok(Self::OR),
            "==" => Ok(Self::EQUAL),
            "!=" => Ok(Self::NOTEQUAL),
            "<" => Ok(Self::LESSTHAN),
            ">" => Ok(Self::GREATERTHAN),
            "<=" => Ok(Self::LESSEQUAL),
            ">=" => Ok(Self::GREATEREQUAL),
            "=" => Ok(Self::ASSIGNMENT),
            _ => Err(format!("Unknown operator: {}", s)),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    INT,
    VOID,
    RETURN,
}

impl Keyword {
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    fn find_match(input: &str) -> Option<Self> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^(int|void|return)\b";
            Regex::new(pattern).unwrap()
        });
        RE.find(input).map(|m| Self::from_str(m.as_str()).unwrap())
    }
}

impl AsRef<str> for Keyword {
    fn as_ref(&self) -> &str {
        match self {
            Self::INT => "int",
            Self::VOID => "void",
            Self::RETURN => "return",
        }
    }
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Self::INT),
            "void" => Ok(Self::VOID),
            "return" => Ok(Self::RETURN),
            _ => Err(format!("Unknown keyword: {}", s)),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
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

impl Delimiter {
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    fn find_match(input: &str) -> Option<Self> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^[\(\);\{\}]";
            Regex::new(pattern).unwrap()
        });
        RE.find(input).map(|m| Self::from_str(m.as_str()).unwrap())
    }
}

impl AsRef<str> for Delimiter {
    fn as_ref(&self) -> &str {
        match self {
            Self::LPAREN => "(",
            Self::RPAREN => ")",
            Self::SEMICOLON => ";",
            Self::LBRACE => "{",
            Self::RBRACE => "}",
        }
    }
}

impl FromStr for Delimiter {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(Self::LPAREN),
            ")" => Ok(Self::RPAREN),
            ";" => Ok(Self::SEMICOLON),
            "{" => Ok(Self::LBRACE),
            "}" => Ok(Self::RBRACE),
            _ => Err(format!("Unknown delimiter: {}", s)),
        }
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    CONSTANT(u64),
    KEYWORD(Keyword),
    IDENTIFIER(String),
    OPERATOR(Operator),
    DELIMITER(Delimiter),
}

impl Token {
    pub fn is_binary_operator(&self) -> bool {
        match self {
            Token::OPERATOR(op) => op.is_binary(),
            _ => false,
        }
    }

    pub fn precedence(&self) -> u64 {
        match self {
            Token::OPERATOR(op) => op.precedence(),
            _ => 0,
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::CONSTANT(c) => c.to_string().len(),
            Self::DELIMITER(d) => d.len(),
            Self::IDENTIFIER(i) => i.len(),
            Self::KEYWORD(k) => k.len(),
            Self::OPERATOR(o) => o.len(),
        }
    }

    fn match_token(input: &str) -> Option<Self> {
        Keyword::find_match(input)
            .map(|kw| Token::KEYWORD(kw))
            .or_else(|| Self::match_identifier(input))
            .or_else(|| Self::match_constant(input))
            .or_else(|| Delimiter::find_match(input).map(|delim| Token::DELIMITER(delim)))
            .or_else(|| Operator::find_match(input).map(|op| Token::OPERATOR(op)))
    }

    fn match_identifier(input: &str) -> Option<Token> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^([a-zA-Z]\w*)\b";
            Regex::new(pattern).unwrap()
        });
        RE.find(input)
            .map(|m| Token::IDENTIFIER(String::from(m.as_str())))
    }

    fn match_constant(input: &str) -> Option<Token> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^([0-9]+)\b";
            Regex::new(pattern).unwrap()
        });
        RE.find(input)
            .map(|m| Token::CONSTANT(m.as_str().parse().unwrap()))
    }
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

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let chars = input.chars().collect::<Vec<char>>();
    let mut tokens = Vec::new();
    let mut i = 0;

    while i < input.len() {
        while chars.get(i).is_some_and(|c| c.is_whitespace()) {
            i += 1;
        }
        if input.get(i..).is_some_and(|s| !s.is_empty()) {
            if let Some(t) = Token::match_token(&input[i..]) {
                i += t.len();
                tokens.push(t);
            } else {
                let pre = &input[i..cmp::min(i + 25, chars.len())];
                return Err(format!("Unrecognized token starting at: '{pre}...'"));
            };
        };
    }
    Ok(tokens)
}
