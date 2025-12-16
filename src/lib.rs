use std::{fs, path::Path, process, str::FromStr};
use tempfile::{Builder, NamedTempFile};
use tracing::info;

pub mod assembly;
pub mod lexer;
pub mod parser;
pub mod tokens;

/// Preprocess the .c file into a .i file.
fn preprocess(input: &Path) -> Result<NamedTempFile, String> {
    info!("Preprocessing with gcc...");

    let tmp = Builder::new()
        .suffix(".i")
        .tempfile()
        .map_err(|e| format!("Failed to create temp file for preprocessed output: {e}"))?;

    let status = process::Command::new("gcc")
        .args(["-E", "-P"])
        .arg(input)
        .arg("-o")
        .arg(tmp.path())
        .status()
        .map_err(|e| format!("Preprocessing failed: {e}"))?;

    if status.success() {
        Ok(tmp)
    } else {
        Err(format!("GCC preprocessing failed with status: {status}"))
    }
}

/// Compilation steps where the compiler can stop early.
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CompileStep {
    LEX,
    PARSE,
    CODEGEN,
    TACKY,
}

impl FromStr for CompileStep {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" => Ok(CompileStep::LEX),
            "parse" => Ok(CompileStep::PARSE),
            "codegen" => Ok(CompileStep::CODEGEN),
            "tacky" => Ok(CompileStep::TACKY),
            _ => Err(format!("Unknown compile step: {}", s)),
        }
    }
}

/// Compile the preprocessed .i file, optionally stopping after a specified step.
fn compile(input: &Path, stop_after: Option<CompileStep>) -> Result<Option<NamedTempFile>, String> {
    info!("Compiling...");

    if input.extension().and_then(|s| s.to_str()).unwrap_or("") != "i" {
        return Err("Input file must have a .i extension".to_string());
    };
    let content = match fs::read_to_string(input) {
        Ok(c) => String::from(c.trim()),
        Err(e) => return Err(format!("Failed to read input file: {e}")),
    };

    info!("Lexing...");
    let tokens = lexer::lex(&content)?;
    if matches!(stop_after, Some(CompileStep::LEX)) {
        return Ok(None);
    }

    info!("Parsing...");
    let program = parser::parse(tokens)?;
    if matches!(stop_after, Some(CompileStep::PARSE)) {
        return Ok(None);
    };

    // let tacky = parser::to_tacky(&program?);
    if matches!(stop_after, Some(CompileStep::TACKY)) {
        return Ok(None);
    }

    info!("Generating assembly instructions...");

    let assembly = assembly::Program::from(program);
    if matches!(stop_after, Some(CompileStep::CODEGEN)) {
        return Ok(None);
    };

    info!("Writing assembly to temporary .s file...");

    let tmp = match Builder::new().suffix(".s").tempfile() {
        Ok(file) => file,
        Err(e) => Err(format!("Failed to create assembly output file: {e}"))?,
    };
    match fs::write(tmp.path(), assembly.to_string()) {
        Ok(_) => {
            info!("Assembly written to {}", tmp.path().display());
            Ok(Some(tmp))
        }
        Err(e) => Err(format!("Failed to write assembly to file: {e}")),
    }
}

/// Link the generated assembly into an executable.
fn link(input: &Path, output: &Path) -> Result<(), String> {
    info!("Linking with gcc...");

    let status = process::Command::new("gcc")
        .arg(input)
        .arg("-o")
        .arg(output)
        .status()
        .map_err(|e| format!("GCC command failed: {}", e))?;

    if status.success() {
        info!("Linking completed successfully.");
        Ok(())
    } else {
        Err(format!("GCC linking failed with status: {}", status))
    }
}

/// Build the C source file into an executable, optionally stopping after a specified step.
pub fn build(input: &Path, stop_after: Option<CompileStep>) -> Result<(), String> {
    info!("Building...");

    let name = match input.file_stem() {
        Some(stem) => stem,
        None => Err("Failed to get input file stem.")?,
    };
    let temp = preprocess(input);

    if let Some(src) = compile(temp?.path(), stop_after)? {
        let output = input.parent().unwrap_or(Path::new("/")).join(name);
        link(src.path(), &output)?;
    };
    Ok(())
}
