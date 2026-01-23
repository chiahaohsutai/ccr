use regex::Regex;

use std::sync::LazyLock;

static RE: LazyLock<Regex> = LazyLock::new(|| {
    let pattern = r"^(?:(?:continue|switch|default|return|while|break|void|case|else|goto|for|int|if|do)\b|(?:[a-zA-Z_]\w*)\b|(?:[0-9]+)\b|<<=|>>=|\+=|-=|\/=|\*=|%=|&=|\|=|\^=|<=|>=|--|\+\+|<<|>>|&&|\|\||==|!=|[,;:\?\(\)\{\}\-~\+\*\/%&\|\^!><=])";
    Regex::new(pattern).unwrap()
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    PlusPlus,           // ++
    MinusMinus,         // --
    Bang,               // !
    Tilde,              // ~
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Percent,            // %
    Amp,                // &
    Pipe,               // |
    Caret,              // ^
    Shl,                // <<
    Shr,                // >>
    AmpAmp,             // &&
    PipePipe,           // ||
    EqEq,               // ==
    NotEq,              // !=
    Lt,                 // <
    Gt,                 // >
    Le,                 // <=
    Ge,                 // >=
    Eq,                 // =
    PlusEq,             // +=
    MinusEq,            // -=
    StarEq,             // *=
    SlashEq,            // /=
    PercentEq,          // %=
    AmpEq,              // &=
    PipeEq,             // |=
    CaretEq,            // ^=
    ShlEq,              // <<=
    ShrEq,              // >>=
    LParen,             // (
    RParen,             // )
    LBrace,             // {
    RBrace,             // }
    Semicolon,          // ;
    Colon,              // :
    Eroteme,            // ?
    Comma,              // ,
    If,                 // if
    Else,               // else
    Int,                // int
    Void,               // void
    Return,             // return
    Goto,               // goto
    Do,                 // do
    While,              // while
    For,                // for
    Break,              // break
    Continue,           // continue
    Switch,             // switch
    Case,               // case
    Default,            // default
    Constant(u64),      // constant
    Identifier(String), // identifier
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        match s {
            "++" => Self::PlusPlus,
            "--" => Self::MinusMinus,
            "!" => Self::Bang,
            "~" => Self::Tilde,
            "+" => Self::Plus,
            "-" => Self::Minus,
            "*" => Self::Star,
            "/" => Self::Slash,
            "%" => Self::Percent,
            "&" => Self::Amp,
            "|" => Self::Pipe,
            "^" => Self::Caret,
            "<<" => Self::Shl,
            ">>" => Self::Shr,
            "&&" => Self::AmpAmp,
            "||" => Self::PipePipe,
            "==" => Self::EqEq,
            "!=" => Self::NotEq,
            "<" => Self::Lt,
            ">" => Self::Gt,
            "<=" => Self::Le,
            ">=" => Self::Ge,
            "=" => Self::Eq,
            "+=" => Self::PlusEq,
            "-=" => Self::MinusEq,
            "*=" => Self::StarEq,
            "/=" => Self::SlashEq,
            "%=" => Self::PercentEq,
            "&=" => Self::AmpEq,
            "|=" => Self::PipeEq,
            "^=" => Self::CaretEq,
            "<<=" => Self::ShlEq,
            ">>=" => Self::ShrEq,
            "(" => Self::LParen,
            ")" => Self::RParen,
            "{" => Self::LBrace,
            "}" => Self::RBrace,
            ";" => Self::Semicolon,
            ":" => Self::Colon,
            "?" => Self::Eroteme,
            "," => Self::Comma,
            "if" => Self::If,
            "else" => Self::Else,
            "int" => Self::Int,
            "void" => Self::Void,
            "return" => Self::Return,
            "Goto" => Self::Goto,
            "Do" => Self::Do,
            "While" => Self::While,
            "For" => Self::For,
            "Break" => Self::Break,
            "Continue" => Self::Continue,
            "Switch" => Self::Switch,
            "Case" => Self::Case,
            "Default" => Self::Default,
            s => match s.parse::<u64>() {
                Ok(constant) => Self::Constant(constant),
                Err(_) => Self::Identifier(String::from(s)),
            },
        }
    }
}

fn is_whitespace<T: AsRef<str>>(s: T) -> bool {
    s.as_ref().chars().all(|c| c.is_whitespace())
}

pub fn tokenize<T: AsRef<str>>(input: T) -> Result<Vec<Token>, String> {
    let input = input.as_ref();

    let mut i = 0;
    let mut tokens = vec![];

    while i < input.len() {
        while i < input.len() && is_whitespace(&input[i..i + 1]) {
            i += 1
        }
        if i < input.len() {
            RE.find(&input[i..])
                .map(|m| {
                    let token = Token::from(m.as_str());
                    tokens.push(token);
                    i += m.as_str().len()
                })
                .ok_or(format!("Unrecognized token starting here at index {i}"))?;
        }
    }
    Ok(tokens)
}
