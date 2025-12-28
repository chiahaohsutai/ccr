use std::{ffi, fs, path, process, str};
use tempfile::NamedTempFile;

pub mod analysis;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod tacky;
pub mod tokens;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CompileStep {
    LEX,
    PARSE,
    CODEGEN,
    TACKY,
    VALIDATE,
}

impl str::FromStr for CompileStep {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" => Ok(CompileStep::LEX),
            "parse" => Ok(CompileStep::PARSE),
            "codegen" => Ok(CompileStep::CODEGEN),
            "tacky" => Ok(CompileStep::TACKY),
            "validate" => Ok(CompileStep::VALIDATE),
            _ => Err(format!("Unknown compile step: {}", s)),
        }
    }
}

fn compile<T: AsRef<path::Path>>(
    input: T,
    stop_after: Option<CompileStep>,
) -> Result<Option<AssemblyFile>, String> {
    let content = fs::read_to_string(input).map_err(|e| e.to_string())?;

    let tokens = lexer::lex(&content)?;
    if matches!(stop_after, Some(CompileStep::LEX)) {
        return Ok(None);
    }

    let program = parser::parse(tokens)?;
    if matches!(stop_after, Some(CompileStep::PARSE)) {
        return Ok(None);
    };

    let tac = tacky::Program::from(program.clone());
    if matches!(stop_after, Some(CompileStep::TACKY)) {
        return Ok(None);
    }

    let instructions = codegen::Program::from(tac);
    if matches!(stop_after, Some(CompileStep::CODEGEN)) {
        return Ok(None);
    };

    let file: AssemblyFile = AssemblyFile::new()?;
    fs::write(&file, instructions.to_string()).map_err(|e| e.to_string())?;

    Ok(Some(file))
}

#[derive(Debug)]
struct AssemblyFile(NamedTempFile);

impl AssemblyFile {
    fn new() -> Result<Self, String> {
        match NamedTempFile::with_suffix(".s") {
            Ok(file) => Ok(AssemblyFile(file)),
            Err(err) => Err(format!("Failed to create intermediate file: {err}")),
        }
    }
}

impl AsRef<ffi::OsStr> for AssemblyFile {
    fn as_ref(&self) -> &ffi::OsStr {
        self.0.path().as_os_str()
    }
}

impl AsRef<path::Path> for AssemblyFile {
    fn as_ref(&self) -> &path::Path {
        self.0.path()
    }
}

#[derive(Debug)]
struct IntermediateFile(NamedTempFile);

impl IntermediateFile {
    fn new() -> Result<Self, String> {
        match NamedTempFile::with_suffix(".i") {
            Ok(file) => Ok(IntermediateFile(file)),
            Err(err) => Err(format!("Failed to create assembly file: {err}")),
        }
    }
}

impl AsRef<ffi::OsStr> for IntermediateFile {
    fn as_ref(&self) -> &ffi::OsStr {
        self.0.path().as_os_str()
    }
}

impl AsRef<path::Path> for IntermediateFile {
    fn as_ref(&self) -> &path::Path {
        self.0.path()
    }
}

/// Build the C source file into an executable, optionally stopping after a specified step.
pub fn build(input: &path::Path, stop_after: Option<CompileStep>) -> Result<(), String> {
    let inter: IntermediateFile = IntermediateFile::new()?;

    let mut cmd = process::Command::new("gcc");
    let cmd = cmd.args(["-E", "-P"]).arg(input).arg("-o").arg(&inter);

    let inter = match cmd.status() {
        Ok(status) if status.success() => Ok(inter),
        Ok(status) => Err(format!("Preprocessing failed with exit status: {status}")),
        Err(e) => Err(format!("Failed to execute preprocessing command: {e}")),
    };

    match compile(inter?, stop_after)? {
        Some(file) => {
            let out = input.parent().unwrap().join(input.file_name().unwrap());

            let mut cmd = process::Command::new("gcc");
            let cmd = cmd.arg(file).arg("-o").arg(out);

            match cmd.status() {
                Ok(status) if status.success() => Ok(()),
                Ok(status) => Err(format!("Linking failed with exit status: {status}")),
                Err(err) => Err(format!("Linking command failed: {err}")),
            }
        }
        None => Ok(()),
    }
}
