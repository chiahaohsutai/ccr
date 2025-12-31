use std::str::FromStr;
use std::{cmp, fmt, sync};

use regex::Regex;

/// Represents all supported operators in the language.
///
/// This enum includes arithmetic, logical, bitwise, comparison, shift,
/// and assignment operators as they appear in source code.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Decrement,
    Negation,
    Complement,
    Addition,
    Product,
    Division,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
    Assignment,
    AddAssignment,
    SubAssignment,
    DivAssignment,
    ProdAssignment,
    RemAssignment,
    AndAssignment,
    OrAssignment,
    XorAssignment,
    LShiftAssignment,
    RShiftAssignment,
}

impl Operator {
    /// Returns the length of the operator's textual representation.
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    /// Attempts to match an operator at the start of the input string.
    ///
    /// Matches the longest valid operator sequence and returns the corresponding
    /// operator if successful. Returns `None` if the input does not begin with
    /// a recognized operator.
    fn find_match(input: &str) -> Option<Self> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let p = r"^(<<=|>>=|\+=|-=|/=|\*=|%=|&=|\|=|\^=|<=|>=|--|<<|>>|&&|\|\||==|!=|[\-~\+\*\/%&\|\^!><=])";
            Regex::new(p).unwrap()
        });
        RE.find(input).map(|m| Self::from_str(m.as_str()).unwrap())
    }

    /// Returns `true` if this operator is a unary operator.
    ///
    /// Unary operators operate on a single operand.
    fn is_unary(&self) -> bool {
        match self {
            Self::Decrement | Self::Complement | Self::LogicalNot => true,
            _ => false,
        }
    }

    /// Returns `true` if this operator is a binary operator.
    ///
    /// Binary operators operate on two operands.
    fn is_binary(&self) -> bool {
        !self.is_unary()
    }

    /// Returns `true` if this operator is a assignment or compound assignment operator
    fn is_assignment(&self) -> bool {
        matches!(
            self,
            Self::Assignment
                | Self::AddAssignment
                | Self::SubAssignment
                | Self::DivAssignment
                | Self::RemAssignment
                | Self::ProdAssignment
                | Self::AndAssignment
                | Self::OrAssignment
                | Self::XorAssignment
                | Self::RShiftAssignment
                | Self::LShiftAssignment
        )
    }

    /// Returns the precedence level of this operator.
    ///
    /// Higher values indicate higher precedence. Operators that do not participate
    /// in expression precedence return `0`.
    fn precedence(&self) -> u64 {
        match self {
            Self::Assignment
            | Self::SubAssignment
            | Self::DivAssignment
            | Self::AddAssignment
            | Self::RemAssignment
            | Self::ProdAssignment
            | Self::AndAssignment
            | Self::OrAssignment
            | Self::XorAssignment
            | Self::LShiftAssignment
            | Self::RShiftAssignment => 1,
            Self::LogicalOr => 5,
            Self::LogicalAnd => 10,
            Self::BitwiseOr => 15,
            Self::BitwiseXor => 20,
            Self::BitwiseAnd => 25,
            Self::EqualEqual | Self::NotEqual => 30,
            Self::LessThan | Self::GreaterThan | Self::LessThanOrEq | Self::GreaterThanOrEq => 35,
            Self::RightShift | Self::LeftShift => 40,
            Self::Addition | Self::Negation => 45,
            Self::Product | Self::Division | Self::Remainder => 50,
            _ => 0,
        }
    }
}

impl AsRef<str> for Operator {
    fn as_ref(&self) -> &str {
        match self {
            Self::Decrement => "--",
            Self::Negation => "-",
            Self::Complement => "~",
            Self::Addition => "+",
            Self::Product => "*",
            Self::Division => "/",
            Self::Remainder => "%",
            Self::BitwiseAnd => "&",
            Self::BitwiseOr => "|",
            Self::BitwiseXor => "^",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::LogicalNot => "!",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::EqualEqual => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessThanOrEq => "<=",
            Self::GreaterThanOrEq => ">=",
            Self::Assignment => "=",
            Self::AddAssignment => "+=",
            Self::SubAssignment => "-=",
            Self::DivAssignment => "/=",
            Self::ProdAssignment => "*=",
            Self::RemAssignment => "%=",
            Self::AndAssignment => "&=",
            Self::OrAssignment => "|=",
            Self::XorAssignment => "^=",
            Self::LShiftAssignment => "<<=",
            Self::RShiftAssignment => ">>=",
        }
    }
}

impl FromStr for Operator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "--" => Ok(Self::Decrement),
            "-" => Ok(Self::Negation),
            "~" => Ok(Self::Complement),
            "+" => Ok(Self::Addition),
            "*" => Ok(Self::Product),
            "/" => Ok(Self::Division),
            "%" => Ok(Self::Remainder),
            "&" => Ok(Self::BitwiseAnd),
            "|" => Ok(Self::BitwiseOr),
            "^" => Ok(Self::BitwiseXor),
            "<<" => Ok(Self::LeftShift),
            ">>" => Ok(Self::RightShift),
            "!" => Ok(Self::LogicalNot),
            "&&" => Ok(Self::LogicalAnd),
            "||" => Ok(Self::LogicalOr),
            "==" => Ok(Self::EqualEqual),
            "!=" => Ok(Self::NotEqual),
            "<" => Ok(Self::LessThan),
            ">" => Ok(Self::GreaterThan),
            "<=" => Ok(Self::LessThanOrEq),
            ">=" => Ok(Self::GreaterThanOrEq),
            "=" => Ok(Self::Assignment),
            "+=" => Ok(Self::AddAssignment),
            "-=" => Ok(Self::SubAssignment),
            "*=" => Ok(Self::ProdAssignment),
            "/=" => Ok(Self::DivAssignment),
            "%=" => Ok(Self::RemAssignment),
            "&=" => Ok(Self::AndAssignment),
            "|=" => Ok(Self::OrAssignment),
            "^=" => Ok(Self::XorAssignment),
            ">>=" => Ok(Self::RShiftAssignment),
            "<<=" => Ok(Self::LShiftAssignment),
            _ => Err(format!("Unknown operator: {}", s)),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

/// Represents reserved keywords in the language.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

impl Keyword {
    /// Returns the length of the keyword's textual representation.
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    /// Attempts to match a keyword at the start of the input string.
    ///
    /// If the input begins with a reserved keyword, returns the corresponding
    /// keyword value. Otherwise, returns `None`.
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
            Self::Int => "int",
            Self::Void => "void",
            Self::Return => "return",
        }
    }
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Self::Int),
            "void" => Ok(Self::Void),
            "return" => Ok(Self::Return),
            _ => Err(format!("Unknown keyword: {}", s)),
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

/// Represents single-character delimiter tokens used for grouping and separation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter {
    LeftParen,
    RightParen,
    Semicolon,
    LeftBrace,
    RightBrace,
}

impl Delimiter {
    /// Returns the length of the delimiter's textual representation.
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    /// Attempts to match a delimiter at the start of the input string.
    ///
    /// If the input begins with a valid delimiter character, returns the
    /// corresponding delimiter value. Otherwise, returns `None`.
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
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::Semicolon => ";",
            Self::LeftBrace => "{",
            Self::RightBrace => "}",
        }
    }
}

impl FromStr for Delimiter {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(Self::LeftParen),
            ")" => Ok(Self::RightParen),
            ";" => Ok(Self::Semicolon),
            "{" => Ok(Self::LeftBrace),
            "}" => Ok(Self::RightBrace),
            _ => Err(format!("Unknown delimiter: {}", s)),
        }
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

/// Represents a lexical token produced by the tokenizer.
///
/// A token corresponds to the smallest meaningful unit in the source code
/// and is later consumed by the parser to construct higher-level syntax.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Constant(u64),
    Keyword(Keyword),
    Identifier(String),
    Operator(Operator),
    Delimiter(Delimiter),
}

impl Token {
    /// Returns `true` if this token represents a binary operator.
    ///
    /// Non-operator tokens and unary operators return `false`.
    pub fn is_binary_operator(&self) -> bool {
        match self {
            Self::Operator(op) => op.is_binary(),
            _ => false,
        }
    }

    /// Returns `true` if this token represents a assignment or compound assignment operator.
    ///
    /// Non-operator tokens return `false`.
    pub fn is_assignment_operator(&self) -> bool {
        match self {
            Self::Operator(op) => op.is_assignment(),
            _ => false,
        }
    }

    /// Returns the precedence level of this token.
    ///
    /// If the token is an operator, its precedence value is returned.
    /// Non-operator tokens have a precedence of `0`.
    pub fn precedence(&self) -> u64 {
        match self {
            Self::Operator(op) => op.precedence(),
            _ => 0,
        }
    }

    /// Returns the length of the token's textual representation.
    ///
    /// The length corresponds to the number of characters consumed from
    /// the input source when this token was lexed.
    fn len(&self) -> usize {
        match self {
            Self::Constant(c) => c.to_string().len(),
            Self::Delimiter(d) => d.len(),
            Self::Identifier(i) => i.len(),
            Self::Keyword(k) => k.len(),
            Self::Operator(o) => o.len(),
        }
    }
    /// Attempts to match a token at the start of the input string.
    ///
    /// Returns the first token whose pattern matches the beginning of `input`,
    /// or `None` if no valid token is found.
    fn match_token(input: &str) -> Option<Self> {
        Keyword::find_match(input)
            .map(|kw| Self::Keyword(kw))
            .or_else(|| Self::match_identifier(input))
            .or_else(|| Self::match_constant(input))
            .or_else(|| Operator::find_match(input).map(|op| Token::Operator(op)))
            .or_else(|| Delimiter::find_match(input).map(|delim| Token::Delimiter(delim)))
    }

    /// Attempts to match an identifier at the start of the input string.
    ///
    /// An identifier must begin with an alphabetic character followed by
    /// alphanumeric characters or underscores. Returns a `Token::Identifier`
    /// containing the matched lexeme if successful, or `None` otherwise.
    fn match_identifier(input: &str) -> Option<Token> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^([a-zA-Z]\w*)\b";
            Regex::new(pattern).unwrap()
        });
        RE.find(input)
            .map(|m| Token::Identifier(String::from(m.as_str())))
    }

    /// Attempts to match an integer constant at the start of the input string.
    ///
    /// Matches a sequence of decimal digits and returns a `Token::Constant`
    /// containing the parsed value if successful, or `None` if no match is found.
    fn match_constant(input: &str) -> Option<Token> {
        static RE: sync::LazyLock<Regex> = sync::LazyLock::new(|| {
            let pattern = r"^([0-9]+)\b";
            Regex::new(pattern).unwrap()
        });
        RE.find(input)
            .map(|m| Token::Constant(m.as_str().parse().unwrap()))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Constant(value) => write!(f, "{}", value),
            Self::Keyword(kw) => write!(f, "{}", kw),
            Self::Identifier(name) => write!(f, "{}", name),
            Self::Operator(op) => write!(f, "{}", op),
            Self::Delimiter(delim) => write!(f, "{}", delim),
        }
    }
}

/// Tokenizes the input source string into a sequence of tokens.
///
/// Iterates through the input, skipping whitespace and repeatedly matching
/// the longest valid token at the current position. Returns a vector of tokens
/// in source order if successful.
///
/// # Errors
/// Returns an error if no valid token can be matched at the current position,
/// including a short preview of the unrecognized input.
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
