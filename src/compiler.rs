use std::str::FromStr;

pub mod parser;
pub mod tokenizer;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Stage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
}

impl FromStr for Stage {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" => Ok(Self::Lex),
            "parse" => Ok(Self::Parse),
            "tacky" => Ok(Self::Tacky),
            "codegen" => Ok(Self::Codegen),
            "validate" => Ok(Self::Validate),
            stage => Err(format!("Invalid compiler stage: {stage}")),
        }
    }
}

pub fn assemble<T: AsRef<str>>(
    input: T,
    stop_after: Option<Stage>,
) -> Result<Option<String>, String> {
    let tokens = tokenizer::tokenize(input)?;
    if matches!(stop_after, Some(Stage::Lex)) {
        return Ok(None);
    }
    todo!()
}
