use super::tokens::{Delimiter, Keyword, Token};
use regex::Regex;
use tracing::info;
use std::process;

const IDENTIFIER_PATTERN: &str = r"[a-zA-Z]\w*\b";
const CONSTANT_PATTERN: &str = r"[0-9]+\b";
const KEYWORD_PATTERN: &str = r"int|void|return\b";
const DELIMITER_PATTERN: &str = r"[\(\);\{\}]";

struct Match(Token, usize);

/// Match the prefix of parameter `s` against the given regex `pattern`.
fn match_prefix(s: &str, pattern: &str) -> Option<String> {
    let pattern = format!(r"^{}", pattern);
    let re = Regex::new(&pattern).unwrap();

    // eprintln!("Trying to match '{}' against pattern '{}'", s, pattern);
    re.find(s).map(|m| String::from(m.as_str()))
}

/// Try to match an identifier or keyword at the start of parameter `s`.
fn match_identifier(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, IDENTIFIER_PATTERN)?;
    let length = candidate.len();
    // eprint!("Matched identifier candidate: '{}'\n", candidate);

    if Regex::new(KEYWORD_PATTERN).unwrap().is_match(&candidate) {
        let kw = Keyword::from(candidate.as_str());
        Some(Match(Token::KEYWORD(kw), length))
    } else {
        Some(Match(Token::IDENTIFIER(candidate), length))
    }
}

/// Try to match a constant at the start of parameter parameter `s`.
fn match_constant(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, CONSTANT_PATTERN)?;
    let length = candidate.len();

    let value: i64 = candidate.parse().ok()?;
    Some(Match(Token::CONSTANT(value), length))
}

/// Try to match a delimiter at the start of parameter `s`.
fn match_delimiter(s: &str) -> Option<Match> {
    let candidate = match_prefix(s, DELIMITER_PATTERN)?;
    let length = candidate.len();

    let delim = Delimiter::from(candidate.as_str());
    Some(Match(Token::DELIMITER(delim), length))
}

/// Try to match any token at the start of parameter `s`.
fn tokenize(s: &str) -> Option<Match> {
    match_identifier(s)
        .or_else(|| match_constant(s))
        .or_else(|| match_delimiter(s))
}

/// Lex the input C source code into a vector of tokens.
pub fn lex(input: &str) -> Vec<Token> {
    info!("Starting lexing process...");
    let mut tokens = Vec::new();
    let mut i = 0;

    while i < input.len() {
        while i < input.len() && input[i..].starts_with(char::is_whitespace) {
            i += 1;
        }
        if i >= input.len() || input[i..].is_empty() {
            break;
        }

        if let Some(Match(token, length)) = tokenize(&input[i..]) {
            // eprint!("Matched token: {:?}\n", token);
            tokens.push(token);
            i += length;
        } else {
            // eprintln!("Unexpected character: {}", &input[i..]);
            process::exit(1);
        }
    }
    info!("Lexing completed successfully with {} tokens.", tokens.len());
    tokens
}
