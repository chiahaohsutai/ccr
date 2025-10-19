use crate::exit;
use crate::tokens::{Delimiter, Keyword, Token};
use std::collections::VecDeque;

mod vars;
use tracing::info;
pub use vars::{Expression, Function, Identifier, Integer, Program, Statement};

/// Parses identifier production rule.
fn parse_identifier(tokens: &mut VecDeque<Token>) -> Identifier {
    match tokens.pop_front() {
        Some(Token::IDENTIFIER(name)) => Identifier::from(name),
        None => exit("Unexpected end of input while parsing identifier."),
        _ => exit("Expected identifier."),
    }
}

/// Parses integer production rule.
fn parse_integer(tokens: &mut VecDeque<Token>) -> Integer {
    match tokens.pop_front() {
        Some(Token::CONSTANT(number)) => Integer::from(number),
        None => exit("Unexpected end of input while parsing integer."),
        _ => exit("Expected integer constant."),
    }
}

/// Parses expression production rule.
fn parse_expression(tokens: &mut VecDeque<Token>) -> Expression {
    let integer = parse_integer(tokens);
    Expression::INT(integer)
}

/// Parses statement production rule.
fn parse_statement(tokens: &mut VecDeque<Token>) -> Statement {
    match tokens.pop_front() {
        Some(Token::KEYWORD(Keyword::RETURN)) => {
            let expr = parse_expression(tokens);
            match tokens.pop_front() {
                Some(Token::DELIMITER(Delimiter::SEMICOLON)) => Statement::RETURN(expr),
                _ => exit("Expected ';' after return expression."),
            }
        }
        None => exit("Unexpected end of input while parsing statement."),
        _ => exit("Expected 'return' keyword at the start of a statement."),
    }
}

/// Parses function production rule.
fn parse_function(tokens: &mut VecDeque<Token>) -> Function {
    match tokens.pop_front() {
        Some(Token::KEYWORD(Keyword::INT)) => {
            let identifier = parse_identifier(tokens);
            match (tokens.pop_front(), tokens.pop_front(), tokens.pop_front()) {
                (
                    Some(Token::DELIMITER(Delimiter::LPAREN)),
                    Some(Token::KEYWORD(Keyword::VOID)),
                    Some(Token::DELIMITER(Delimiter::RPAREN)),
                ) => match tokens.pop_front() {
                    Some(Token::DELIMITER(Delimiter::LBRACE)) => {
                        let statement = parse_statement(tokens);
                        match tokens.pop_front() {
                            Some(Token::DELIMITER(Delimiter::RBRACE)) => {
                                Function::new(identifier, statement)
                            }
                            _ => exit("Expected '}' at the end of function body."),
                        }
                    }
                    _ => exit("Expected '{' at the start of function body."),
                },
                _ => exit("Expected '(void)' after function name."),
            }
        }
        None => exit("Unexpected end of input while parsing function."),
        _ => exit("Expected 'int' keyword at the start of a function."),
    }
}

/// Parses program production rule.
fn parse_program(tokens: &mut VecDeque<Token>) -> Program {
    let program = Program::from(parse_function(tokens));
    if !tokens.is_empty() {
        exit("Unexpected tokens after parsing complete program.");
    };
    program
}

/// Parses a token list into AST.
pub fn parse(tokens: Vec<Token>) -> Program {
    let mut token_queue: VecDeque<Token> = VecDeque::from(tokens);
    info!("Generating AST...");
    parse_program(&mut token_queue)
}
