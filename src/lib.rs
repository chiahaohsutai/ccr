use std::{fs, process};
use std::path::Path;
use tempfile::{Builder, NamedTempFile};
use tracing::{error, info};

pub mod lexer;
pub mod tokens;


/// Terminate the program with an error message.
pub fn exit<T: AsRef<str>>(message: T) -> ! {
    error!("Exiting: {}", message.as_ref());
    eprintln!("{}", message.as_ref());
    process::exit(1);
}

/// Terminate the program with a success message.
fn done<T: AsRef<str>>(message: T) -> ! {
    info!("{}", message.as_ref());
    process::exit(0);
}

/// Preprocess the .c file into a .i file.
fn preprocess(input: &Path) -> NamedTempFile {
    info!("Starting preprocessing with gcc...");

    let tmp = match Builder::new().suffix(".i").tempfile() {
        Ok(file) => file,
        Err(e) => exit(format!("Failed to create .i file: {e}"))
    };

    let status = process::Command::new("gcc")
        .args(["-E", "-P"])
        .arg(input)
        .arg("-o")
        .arg(tmp.path())
        .status();

    match status {
        Ok(status) if status.success() => tmp,
        _ => exit("Preprocessing failed."),
    }
}

/// Compilation steps where the compiler can stop early.
pub enum CompileStep {
    Lex,
    Parse,
    CodeGen,
}

/// Compile the preprocessed .i file, optionally stopping after a specified step.
fn compile(input: &Path, stop_after: Option<CompileStep>) -> NamedTempFile {
    info!("Starting compilation...");

    if input.extension().and_then(|s| s.to_str()).unwrap_or("") != "i" {
        exit("Invalid input file, file must have .i extension.");
    }
    let _ = match fs::read_to_string(input) {
        Ok(content) => lexer::lex(&content),
        Err(e) => exit(format!("Failed to read preprocessed file: {e}")),
    };
    if matches!(stop_after, Some(CompileStep::Lex)) {
        done("Lexing completed, exiting as requested.");
    }
    todo!();
}

/// Link the generated assembly into an executable.
fn link() {
    todo!();
}

/// Build the entire compilation pipeline, optionally stopping after a specified step.
pub fn build(input: &Path, stop_after: Option<CompileStep>) {
    let intermediate = preprocess(input);
    let _ = compile(intermediate.path(), stop_after);
    link();
    done("Build completed successfully.");
}
