use tempfile::{Builder, NamedTempFile};

use std::{fs, path, process};

pub mod codegen;
pub mod parser;
pub mod tacky;
pub mod tokenizer;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CompileStep {
    LEX,
    PARSE,
    TACKY,
    CODEGEN,
    VALIDATE,
}

fn compile<T: AsRef<path::Path>>(
    input: T,
    stop_after: Option<CompileStep>,
) -> Result<Option<String>, String> {
    let content = fs::read_to_string(input).map_err(|e| e.to_string())?;

    let tokens = tokenizer::tokenize(&content)?;
    if matches!(stop_after, Some(CompileStep::LEX)) {
        return Ok(None);
    }
    let ast = parser::parse(tokens)?;
    if matches!(stop_after, Some(CompileStep::PARSE)) {
        return Ok(None);
    };
    let ast = parser::validate(ast)?;
    if matches!(stop_after, Some(CompileStep::VALIDATE)) {
        return Ok(None);
    }
    let tac = tacky::Program::try_from(ast)?;
    if matches!(stop_after, Some(CompileStep::TACKY)) {
        return Ok(None);
    }
    let instructions = codegen::Program::from(tac);
    if matches!(stop_after, Some(CompileStep::CODEGEN)) {
        return Ok(None);
    };
    Ok(Some(instructions.to_string()))
}

pub fn build(input: &path::Path, stop_after: Option<CompileStep>) -> Result<(), String> {
    let inter = Builder::new()
        .suffix(".i")
        .tempfile()
        .map_err(|e| format!("Failed to '.i' file: {e}"))?;

    let mut cmd = process::Command::new("gcc");
    let cmd = cmd
        .args(["-E", "-P"])
        .arg(input)
        .arg("-o")
        .arg(inter.path());

    let inter: NamedTempFile = match cmd.status() {
        Ok(status) if status.success() => Ok(inter),
        Ok(status) => Err(format!("Preprocessing failed with exit status: {status}")),
        Err(e) => Err(format!("Failed to execute preprocessing command: {e}")),
    }?;

    if let Some(instrs) = compile(inter, stop_after)? {
        let src = Builder::new()
            .suffix(".s")
            .tempfile()
            .map_err(|e| format!("Failed to '.s' file: {e}"))?;

        let dst = input.parent().unwrap().join(input.file_stem().unwrap());
        fs::write(src.path(), instrs).map_err(|e| e.to_string())?;

        let mut cmd = process::Command::new("gcc");
        let cmd = cmd.arg(src.path()).arg("-o").arg(dst);

        match cmd.status() {
            Ok(status) if status.success() => Ok(()),
            Ok(status) => Err(format!("Linking failed with exit status: {status}")),
            Err(err) => Err(format!("Linking command failed: {err}")),
        }
    } else {
        Ok(())
    }
}
