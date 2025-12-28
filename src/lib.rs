use std::{ffi, fs, path, process};
use tempfile::NamedTempFile;

pub mod analysis;
pub mod codegen;
pub mod parser;
pub mod tacky;
pub mod tokenizer;

#[derive(Debug, Copy, Clone, PartialEq)]
enum IntermediateFileType {
    PREPROCESSED,
    ASSEMBLY,
}

impl AsRef<str> for IntermediateFileType {
    fn as_ref(&self) -> &str {
        match self {
            Self::PREPROCESSED => ".i",
            Self::ASSEMBLY => ".s",
        }
    }
}

impl AsRef<ffi::OsStr> for IntermediateFileType {
    fn as_ref(&self) -> &ffi::OsStr {
        let suffix: &str = self.as_ref();
        match self {
            Self::PREPROCESSED => ffi::OsStr::new(suffix),
            Self::ASSEMBLY => ffi::OsStr::new(suffix),
        }
    }
}

#[derive(Debug)]
struct IntermediateFile(NamedTempFile<fs::File>);

impl IntermediateFile {
    fn new(filetype: IntermediateFileType) -> Result<Self, String> {
        match NamedTempFile::with_suffix(&filetype) {
            Ok(file) => Ok(IntermediateFile(file)),
            Err(err) => Err(format!("Failed to create intermediate file: {err}")),
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
) -> Result<Option<IntermediateFile>, String> {
    let content = fs::read_to_string(input).map_err(|e| e.to_string())?;

    let tokens = tokenizer::tokenize(&content)?;
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

    let file: IntermediateFile = IntermediateFile::new(IntermediateFileType::ASSEMBLY)?;
    fs::write(&file, instructions.to_string()).map_err(|e| e.to_string())?;

    Ok(Some(file))
}

/// Build the C source file into an executable, optionally stopping after a specified step.
pub fn build(input: &path::Path, stop_after: Option<CompileStep>) -> Result<(), String> {
    let inter: IntermediateFile = IntermediateFile::new(IntermediateFileType::PREPROCESSED)?;

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
