use clap::{ArgGroup, Command, Id, arg};
use tracing::{Level, error};
use tracing_subscriber::FmtSubscriber;

use std::{path, process};

use ccr::CompileStep;

fn init_loggging(level: Level) {
    let subscriber = FmtSubscriber::builder().with_max_level(level).finish();
    if let Err(e) = tracing::subscriber::set_global_default(subscriber) {
        panic!("Failed to set global default subscriber: {}", e);
    }
}

fn build_cli() -> Command {
    let args = [
        arg!(<path> "Absolute or relative path to C source file"),
        arg!(--lex "Runs the lexer and exits"),
        arg!(--parse "Runs the parser and exits"),
        arg!(--tacky "Generates tacky assembly and exits"),
        arg!(--codegen "Runs assembly generation and exits"),
        arg!(--validate "Runs semantic analysis and exists"),
    ];

    let stop_after = ArgGroup::new("stop_after")
        .args(["lex", "parse", "tacky", "codegen", "validate"])
        .multiple(false)
        .required(false);

    Command::new("CCR")
        .about("A C compiler written in Rust")
        .long_about("A simple C compiler written in Rust for educational purposes.")
        .author("Youf favorite programmer CHIA")
        .alias("ccr")
        .args(args)
        .group(stop_after)
}

fn main() {
    init_loggging(Level::DEBUG);

    let args = build_cli().get_matches();
    let path = path::Path::new(args.get_one::<String>("path").unwrap());

    let stop_after = args
        .get_one::<Id>("stop_after")
        .map(|id| match id.as_str() {
            "lex" => CompileStep::LEX,
            "parse" => CompileStep::PARSE,
            "tacky" => CompileStep::TACKY,
            "codegen" => CompileStep::CODEGEN,
            "validate" => CompileStep::VALIDATE,
            arg => panic!("Unrecognized argument '{arg}'"),
        });

    if path.exists() && path.is_file() {
        if let Err(e) = ccr::build(path, stop_after) {
            error!("Compilation failed: {}", e);
            eprintln!("Compilation failed: {}", e);
            process::exit(1);
        }
    }
}
