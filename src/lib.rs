use std::path::{Path, PathBuf};
use std::{fs, process};

use tempfile::{Builder, NamedTempFile};

pub mod compiler;

pub mod codegen;
pub mod parser;
pub mod tacky;
pub mod tokenizer;

fn preprocess(input: &Path) -> Result<NamedTempFile, String> {
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

    match cmd.status() {
        Ok(status) if status.success() => Ok(inter),
        Ok(status) => Err(format!("Preprocessing failed with exit status: {status}")),
        Err(e) => Err(format!("Failed to execute preprocessing command: {e}")),
    }
}

fn compile(input: &Path, output: &Path) -> Result<(), String> {
    let mut cmd = process::Command::new("gcc");
    cmd.arg("-c").arg(input).arg("-o").arg(output);
    match cmd.status() {
        Ok(status) if status.success() => Ok(()),
        Ok(status) => Err(format!("Compilation failed with exit status: {status}")),
        Err(err) => Err(format!("Compilation command failed: {err}")),
    }
}

fn link(input: &Path, output: &Path) -> Result<(), String> {
    let mut cmd = process::Command::new("gcc");
    cmd.arg(input).arg("-o").arg(output);
    match cmd.status() {
        Ok(status) if status.success() => Ok(()),
        Ok(status) => Err(format!("Linking failed with exit status: {status}")),
        Err(err) => Err(format!("Linking command failed: {err}")),
    }
}

pub fn build(
    input: &Path,
    stop_after: Option<compiler::Stage>,
    compile_only: bool,
) -> Result<(), String> {
    let contents = fs::read_to_string(preprocess(input)?.path()).map_err(|e| e.to_string())?;
    let instructions = compiler::assemble(contents, stop_after)?;

    if let Some(instructions) = instructions {
        let s = Builder::new()
            .suffix(".s")
            .tempfile()
            .map_err(|e| format!("Failed to create '.s' file: {e}"))?;

        fs::write(s.path(), instructions).map_err(|e| e.to_string())?;

        if compile_only {
            let output = Path::new(input.file_stem().unwrap()).with_extension("o");
            compile(s.path(), &output)
        } else {
            let output = Path::new(input.file_stem().unwrap());
            link(s.path(), &output)
        }
    } else {
        Ok(())
    }
}
