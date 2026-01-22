use std::str::FromStr;

mod tokenizer;

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

fn compile<T: AsRef<str>>(input: T, stop_after: Option<Stage>) -> Result<Option<String>, String> {
    todo!()
}
