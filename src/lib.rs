use std::path::Path;
use std::{fs, process};

use tempfile::{Builder, NamedTempFile};

mod cli;
mod compiler;

pub fn get_args() -> cli::Args {
    cli::get_matches()
}

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

pub fn build(args: cli::Args) -> Result<(), String> {
    let path = Path::new(&args.path);
    let contents = fs::read_to_string(preprocess(path)?.path()).map_err(|e| e.to_string())?;

    let instructions = compiler::assemble(contents, args.stop_after)?;

    if let Some(instructions) = instructions {
        let s = Builder::new()
            .suffix(".s")
            .tempfile()
            .map_err(|e| format!("Failed to create '.s' file: {e}"))?;

        fs::write(s.path(), instructions).map_err(|e| e.to_string())?;

        if let compiler::Target::ObjFile = args.target {
            let output = Path::new(path.file_stem().unwrap()).with_extension("o");
            compile(s.path(), &output)
        } else {
            let output = Path::new(path.file_stem().unwrap());
            link(s.path(), &output)
        }
    } else {
        Ok(())
    }
}
