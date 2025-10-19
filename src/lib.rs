use std::{fs, process};
use std::path::Path;
use tempfile::{Builder, NamedTempFile};
use tracing::{error, info};

pub mod assembly;
pub mod lexer;
pub mod tokens;
pub mod parser;


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
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
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

    info!("Reading preprocessed file: {}", input.display());
    let tokens = match fs::read_to_string(input) {
        Ok(content) => lexer::lex(&content),
        Err(e) => exit(format!("Failed to read preprocessed file: {e}")),
    };
    if matches!(stop_after, Some(CompileStep::Lex)) {
        done("Lexing completed, exiting as requested.");
    }

    info!("Starting parsing...");
    let program = parser::parse(tokens);
    if matches!(stop_after, Some(CompileStep::Parse)) {
        done("Parsing completed, exiting as requested.");
    };

    info!("Starting code generation...");
    let assembly = assembly::Program::from(program);
    if matches!(stop_after, Some(CompileStep::CodeGen)) {
        done("Code generation completed, exiting as requested.");
    };
    
    info!("Writing assembly to temporary .s file...");
    let tmp = match Builder::new().suffix(".s").tempfile() {
        Ok(file) => file,
        Err(e) => exit(format!("Failed to create .s file: {e}"))
    };
    match fs::write(tmp.path(), assembly.to_string()) {
        Ok(_) => tmp,
        Err(e) => exit(format!("Failed to write assembly to file: {e}")),
    }
}

/// Link the generated assembly into an executable.
fn link(input: &Path, output: &Path) {
    info!("Starting linking with gcc...");

    let status = process::Command::new("gcc")
        .arg(input)
        .arg("-o")
        .arg(output)
        .status();

    if status.is_err() || !status.unwrap().success() {
        exit("Linking failed.");
    };
    info!("Program linked successfully to {}", output.display());
}

/// Build the entire compilation pipeline, optionally stopping after a specified step.
pub fn build(input: &Path, stop_after: Option<CompileStep>) {
    let parent = input.parent().unwrap_or_else(|| {
        exit("Failed to get input file parent directory.")
    });
    let name = input.file_stem().unwrap_or_else(|| {
        exit("Failed to get input file stem.")
    });

    let intermediate = preprocess(input);
    let source = compile(intermediate.path(), stop_after);

    let output = parent.join(name);
    link(source.path(), &output);
    done("Build completed successfully.");
}
