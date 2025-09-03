use std::fs;
use std::path::Path;
use std::process;
use tempfile::Builder;
use tracing::info;

pub mod lexer;
pub mod tokens;

fn preprocess(input: &Path, output: &Path) {
    info!("Starting preprocessing with gcc...");

    let input = input.to_str().unwrap();
    let output = output.to_str().unwrap();

    let status = process::Command::new("gcc")
        .args(["-E", "-P", input, "-o", output])
        .status();

    match status {
        Ok(s) if s.success() => info!("Preprocessing completed successfully."),
        Ok(s) => {
            eprintln!("Preprocessing failed with exit code: {}", s);
            process::exit(1);
        }
        Err(e) => {
            eprintln!("Failed to execute gcc for preprocessing: {}", e);
            process::exit(1);
        }
    };
}

pub enum CompileStep {
    Lex,
    Parse,
    CodeGen,
}

fn compile(input: &Path, stop_after: Option<CompileStep>) {
    let source = fs::read_to_string(input);
    if let Err(e) = source {
        eprintln!("Failed to read preprocessed file: {}", e);
        process::exit(1);
    }
    let source = source.unwrap();

    let _ = lexer::lex(&source);
    if matches!(stop_after, Some(CompileStep::Lex)) {
        info!("Lexing completed, exiting as requested.");
        process::exit(0);
    }

    todo!();
}

fn assemble() {
    todo!();
}

fn link() {
    todo!();
}

pub fn build(input: &Path, stop_after: Option<CompileStep>) {
    let tmp = Builder::new().suffix(".i").tempfile();
    let tmp = match tmp {
        Ok(f) => f.path().to_path_buf(),
        Err(e) => {
            eprintln!("Failed to create temporary file for preprocessing: {}", e);
            process::exit(1);
        }
    };
    preprocess(input, &tmp);
    compile(&tmp, stop_after);
    assemble();
    link();
}
