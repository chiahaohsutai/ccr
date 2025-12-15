use crate::tokens::{Delimiter, Keyword, Token};
use std::collections::VecDeque;
use tracing::info;

mod vars;
pub use vars::{Expression, Function, Identifier, Integer, Program, Statement, UnOp};

/// Parses expression production rule.
fn parse_expression(tokens: &mut VecDeque<Token>) -> Result<Expression, String> {
    let next = tokens.pop_front();
    match next {
        Some(Token::CONSTANT(number)) => Ok(Expression::from(number)),
        Some(Token::OPERATOR(op)) => {
            let expr = parse_expression(tokens)?;
            Ok(Expression::UNARY(UnOp::from(op), Box::new(expr)))
        }
        Some(Token::DELIMITER(Delimiter::LPAREN)) => {
            let expr = parse_expression(tokens);
            match tokens.pop_front() {
                Some(Token::DELIMITER(Delimiter::RPAREN)) => expr,
                _ => Err(String::from("Expected ')' after expression.")),
            }
        }
        _ => Err(String::from("Malformed expression.")),
    }
}

/// Parses statement production rule.
fn parse_statement(tokens: &mut VecDeque<Token>) -> Result<Statement, String> {
    match tokens.pop_front() {
        Some(Token::KEYWORD(Keyword::RETURN)) => {
            let expr = parse_expression(tokens)?;
            match tokens.pop_front() {
                Some(Token::DELIMITER(Delimiter::SEMICOLON)) => Ok(Statement::RETURN(expr)),
                _ => Err(String::from("Expected ';' after return expression.")),
            }
        }
        None => Err(String::from("Unexpected end of input while parsing.")),
        _ => Err(String::from("Expected statement.")),
    }
}

/// Parses identifier production rule.
fn parse_identifier(tokens: &mut VecDeque<Token>) -> Result<Identifier, String> {
    match tokens.pop_front() {
        Some(Token::IDENTIFIER(name)) => Ok(Identifier::from(name)),
        None => Err(String::from("Unexpected end of input.")),
        _ => Err(String::from("Expected identifier.")),
    }
}

/// Parses function production rule.
fn parse_function(tokens: &mut VecDeque<Token>) -> Result<Function, String> {
    match tokens.pop_front() {
        Some(Token::KEYWORD(Keyword::INT)) => {
            let identifier = parse_identifier(tokens)?;
            match (tokens.pop_front(), tokens.pop_front(), tokens.pop_front()) {
                (
                    Some(Token::DELIMITER(Delimiter::LPAREN)),
                    Some(Token::KEYWORD(Keyword::VOID)),
                    Some(Token::DELIMITER(Delimiter::RPAREN)),
                ) => match tokens.pop_front() {
                    Some(Token::DELIMITER(Delimiter::LBRACE)) => {
                        let statement = parse_statement(tokens)?;
                        match tokens.pop_front() {
                            Some(Token::DELIMITER(Delimiter::RBRACE)) => {
                                Ok(Function::new(identifier, statement))
                            }
                            _ => Err(String::from("Expected '}' at the end of fn body.")),
                        }
                    }
                    _ => Err(String::from("Expected '{' at the start of fn body.")),
                },
                _ => Err(String::from("Expected '(void)' after fn name.")),
            }
        }
        None => Err(String::from("Unexpected end of input while parsing fn.")),
        _ => Err(String::from("Expected 'int' keyword at the start of a fn.")),
    }
}

/// Parses program production rule.
fn parse_program(tokens: &mut VecDeque<Token>) -> Result<Program, String> {
    let program = Program::from(parse_function(tokens)?);
    if !tokens.is_empty() {
        let err = String::from("Unexpected tokens remaining after parsing program.");
        return Err(err);
    };
    Ok(program)
}

/// Parses a token list into AST.
pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    let mut token_queue: VecDeque<Token> = VecDeque::from(tokens);
    info!("Generating AST...");
    parse_program(&mut token_queue)
}
