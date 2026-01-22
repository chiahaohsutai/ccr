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

fn find(pattern: &str, haystack: &str) -> Result<String, String> {
    todo!()
}

fn tokenize<T: AsRef<str>>(input: T) -> Result<Vec<Token>, String> {
    todo!()
}
