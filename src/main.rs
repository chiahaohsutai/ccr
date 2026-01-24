use std::path::Path;
use std::str::FromStr;

use clap::{ArgGroup, Command, Id, arg};
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

use nora::{CompilerStage, build};

fn init_loggging(level: Level) -> Result<(), String> {
    let subscriber = FmtSubscriber::builder().with_max_level(level).finish();
    tracing::subscriber::set_global_default(subscriber)
        .map_err(|_| format!("Failed to initialize tracing subscriber"))
}

fn cli() -> Command {
    let args = [
        arg!(<path> "Absolute or relative path to C source file"),
        arg!(--lex "Runs the lexer and exits"),
        arg!(--parse "Runs the parser and exits"),
        arg!(--tacky "Generates tacky assembly and exits"),
        arg!(--codegen "Runs assembly generation and exits"),
        arg!(--validate "Runs semantic analysis and exists"),
        arg!("compile": -c "Generates an object file instead of an execuable"),
    ];
    let stop_after = ArgGroup::new("stop_after")
        .args(["lex", "parse", "tacky", "codegen", "validate"])
        .multiple(false)
        .required(false);

    Command::new("NORA")
        .about("A C compiler written in Rust")
        .long_about("A simple C compiler for a subset of C")
        .author("Your favorite programmer CHIA")
        .alias("nora")
        .args(args)
        .group(stop_after)
}

fn main() -> Result<(), String> {
    init_loggging(Level::DEBUG)?;

    let args = cli().get_matches();
    let path = Path::new(args.get_one::<String>("path").unwrap());

    let stop_after = match args.get_one::<Id>("stop_after") {
        Some(stop_after) => Some(CompilerStage::from_str(stop_after.as_str())?),
        None => None,
    };
    let compile_only: &bool = args.get_one("compile").unwrap_or(&false);

    if path.exists() && path.is_file() {
        build(path, stop_after, *compile_only)?;
        Ok(())
    } else {
        Err(format!("Invalid path: {}", path.display()))
    }
}
