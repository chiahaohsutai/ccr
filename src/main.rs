use clap::{ArgGroup, CommandFactory, Parser, FromArgMatches};
use std::{path::Path, process};
use tracing::{info, Level};
use tracing_subscriber::FmtSubscriber;
use ccr::CompileStep;

#[derive(Parser, Debug)]
#[command(name = "CCR", about = "C compiler written in Rust", long_about = None)]
struct Args {
    #[arg(help = "Absolute or relative path to C source file")]
    path: String,

    #[arg(long, help = "Runs the lexer and exits")]
    lex: bool,

    #[arg(long, help = "Runs the parser and exits")]
    parse: bool,

    #[arg(long, help = "Runs assembly generation and exits")]
    codegen: bool,
}

impl Args {
    fn parse() -> Self {
        let cmd = Self::command().group(
            ArgGroup::new("stop_after")
                .args(["lex", "parse", "codegen"])
                .multiple(false)
                .required(false),
        );
        let matches = cmd.get_matches();
        Self::from_arg_matches(&matches).unwrap_or_else(|e| {
            eprintln!("{}", e);
            process::exit(1);
        })
    }
}

fn main() {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();

    if let Err(e) = tracing::subscriber::set_global_default(subscriber) {
        ccr::exit(format!("Failed to set tracing subscriber: {}", e));
    }

    let args = Args::parse();
    let path = Path::new(&args.path);

    if !path.exists() || !path.is_file() {
        ccr::exit("Input path does not exist or is not a file.");
    }

    let stop_after = if args.lex {
        Some(CompileStep::Lex)
    } else if args.parse {
        Some(CompileStep::Parse)
    } else if args.codegen {
        Some(CompileStep::CodeGen)
    } else {
        None
    };

    info!("Executing CCR...");
    ccr::build(path, stop_after);
}
