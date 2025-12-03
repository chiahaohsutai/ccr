use super::tokens::{Delimiter, Keyword, Operator, Token, UnaryOp};
use regex::Regex;
use std::{cmp, process};
use tracing::info;

const IDENTIFIER_PATTERN: &str = r"[a-zA-Z]\w*\b";
const CONSTANT_PATTERN: &str = r"[0-9]+\b";
const KEYWORD_PATTERN: &str = r"int|void|return\b";
const UNARY_OPERATOR_PATTERN: &str = r"--|[\-~]";
const DELIMITER_PATTERN: &str = r"[\(\);\{\}]";

struct Match(Token, usize);

/// Match the prefix of parameter `s` against the given regex `pattern`.
fn match_prefix(s: &str, pattern: &str) -> Option<String> {
    let pattern = format!(r"^{}", pattern);
    let re = Regex::new(&pattern).unwrap();
    re.find(s).map(|m| String::from(m.as_str()))
}

/// Match an identifier or keyword at the start of parameter `s`.
fn match_identifier(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, IDENTIFIER_PATTERN)?;

    if Regex::new(KEYWORD_PATTERN).unwrap().is_match(&candidate) {
        let kw = Keyword::from(candidate.as_str());
        Some(Match(Token::KEYWORD(kw), candidate.len()))
    } else {
        let length = candidate.len();
        Some(Match(Token::IDENTIFIER(candidate), length))
    }
}

/// Match a constant at the start of parameter parameter `s`.
fn match_constant(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, CONSTANT_PATTERN)?;
    let value: i64 = candidate.parse().ok()?;
    Some(Match(Token::CONSTANT(value), candidate.len()))
}

/// Match a unary operator at the start of parameter `s`.
fn match_unary_op(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, UNARY_OPERATOR_PATTERN)?;
    let op = Operator::UNARY(UnaryOp::from(candidate.as_str()));
    Some(Match(Token::OPERATOR(op), candidate.len()))
}

/// Match a delimiter at the start of parameter `s`.
fn match_delimiter(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, DELIMITER_PATTERN)?;
    let delim = Delimiter::from(candidate.as_str());
    Some(Match(Token::DELIMITER(delim), candidate.len()))
}

/// Match any token at the start of parameter `s`.
fn tokenize(s: &str) -> Option<Match> {
    let matches = [
        match_identifier(s),
        match_constant(s),
        match_unary_op(s),
        match_delimiter(s),
    ];
    matches
        .into_iter()
        .max_by(|m1, m2| match (m1, m2) {
            (Some(Match(_, len1)), Some(Match(_, len2))) => len1.cmp(&len2),
            (Some(_), None) => cmp::Ordering::Greater,
            (None, Some(_)) => cmp::Ordering::Less,
            (None, None) => cmp::Ordering::Equal,
        })
        .flatten()
}

/// Lex the input C source code into a vector of tokens.
pub fn lex(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut i = 0;

    info!("Tokenizing input...");
    while i < input.len() {
        while i < input.len() && input[i..].starts_with(char::is_whitespace) {
            i += 1;
        }
        if i >= input.len() || input[i..].is_empty() {
            break;
        }

        if let Some(Match(token, length)) = tokenize(&input[i..]) {
            tokens.push(token);
            i += length;
        } else {
            process::exit(1);
        }
    }
    info!("Lexing completed successfully with {} tokens", tokens.len());
    tokens
}
