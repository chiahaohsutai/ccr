use super::tokens::{self, Token};
use regex::Regex;
use std::str::FromStr;
use tracing::info;

const IDENTIFIER_PATTERN: &str = r"[a-zA-Z]\w*\b";
const CONSTANT_PATTERN: &str = r"[0-9]+\b";
const KEYWORD_PATTERN: &str = r"int|void|return\b";
const OPERATOR_PATTERN: &str = r"<=|>=|--|<<|>>|&&|\|\||==|!=|[\-~\+\*\/%&\|\^!><]";
const DELIMITER_PATTERN: &str = r"[\(\);\{\}]";

#[derive(Debug, PartialEq, Clone)]
struct Match(Option<Token>, usize);

impl Default for Match {
    fn default() -> Self {
        Match(None, 0)
    }
}

impl Match {
    fn new(token: Token, length: usize) -> Self {
        Match(Some(token), length)
    }
    fn size(&self) -> usize {
        self.1
    }
}

/// Match the prefix of parameter `s` against the given regex `pattern`.
fn match_prefix<'a>(s: &'a str, pattern: &str) -> Result<Option<&'a str>, String> {
    let pattern = format!(r"^({})", pattern);
    match Regex::new(&pattern) {
        Ok(re) => Ok(re.find(s).map(|m| m.as_str())),
        Err(e) => Err(format!("Failed to compile regex pattern '{pattern}': {e}")),
    }
}

/// Match an identifier or keyword at the start of parameter `s`.
fn match_word(s: &str) -> Result<Match, String> {
    if let Some(candidate) = match_prefix(s, KEYWORD_PATTERN)? {
        let kw = tokens::Keyword::from_str(candidate)?;
        Ok(Match::new(Token::KEYWORD(kw), candidate.len()))
    } else if let Some(candidate) = match_prefix(s, IDENTIFIER_PATTERN)? {
        let iden = String::from(candidate);
        Ok(Match::new(Token::IDENTIFIER(iden), candidate.len()))
    } else {
        Ok(Match::default())
    }
}

/// Match a constant at the start of parameter parameter `s`.
fn match_constant(s: &str) -> Result<Match, String> {
    if let Some(candidate) = match_prefix(s, CONSTANT_PATTERN)? {
        let value: u64 = candidate
            .parse()
            .map_err(|e| format!("Failed to parse constant '{candidate}': {e}"))?;
        Ok(Match::new(Token::CONSTANT(value), candidate.len()))
    } else {
        Ok(Match::default())
    }
}

/// Match a unary operator at the start of parameter `s`.
fn match_operator(s: &str) -> Result<Match, String> {
    if let Some(candidate) = match_prefix(s, OPERATOR_PATTERN)? {
        let op = tokens::Operator::from_str(candidate)?;
        Ok(Match::new(Token::OPERATOR(op), candidate.len()))
    } else {
        Ok(Match::default())
    }
}

/// Match a delimiter at the start of parameter `s`.
fn match_delimiter(s: &str) -> Result<Match, String> {
    if let Some(candidate) = match_prefix(s, DELIMITER_PATTERN)? {
        let delim = tokens::Delimiter::from_str(candidate)?;
        Ok(Match::new(Token::DELIMITER(delim), candidate.len()))
    } else {
        Ok(Match::default())
    }
}

/// Match any token at the start of parameter `s`.
fn tokenize(s: &str) -> Result<Match, String> {
    let matches = [
        match_word(s)?,
        match_constant(s)?,
        match_operator(s)?,
        match_delimiter(s)?,
    ];
    let m = matches
        .into_iter()
        .max_by(|m1, m2| m1.size().cmp(&m2.size()));

    match m {
        Some(matched) if matched.size() > 0 => Ok(matched),
        _ => Ok(Match::default()),
    }
}

/// Lex the input C source code into a vector of tokens.
pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let chars = input.chars().collect::<Vec<char>>();

    let mut tokens = Vec::new();
    let mut i = 0;

    info!("Lexing/tokenizing input...");
    while i < input.len() {
        while chars.get(i).is_some_and(|c| c.is_whitespace()) {
            i += 1;
        }
        if input.get(i..).is_some_and(|s| !s.is_empty()) {
            let tkn = tokenize(&input[i..])?;
            if let Match(Some(t), length) = tkn {
                tokens.push(t);
                i += length;
            } else {
                return Err(format!("Unrecognized token starting at: '{}'", &input[i..]));
            };
        };
    }
    Ok(tokens)
}
