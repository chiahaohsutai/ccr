use clap::Parser;
use std::path::Path;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

#[derive(Parser)]
#[command(name = "CCR")]
#[command(about = "C compiler written in Rust", long_about = None)]
struct Args {
    #[arg(help = "Absolute or relative path to C source file")]
    path: String,

    #[arg(long, help = "Runs the lexer and exits")]
    lex: bool,

    #[arg(long, help = "Runs the parser and exits")]
    parse: bool,

    #[arg(long = "code-gen", help = "Runs assembly generation and exits")]
    codegen: bool,
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
        Some(ccr::CompileStep::Lex)
    } else if args.parse {
        Some(ccr::CompileStep::Parse)
    } else if args.codegen {
        Some(ccr::CompileStep::CodeGen)
    } else {
        None
    };
    ccr::build(path, stop_after);
}
