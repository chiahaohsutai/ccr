# CCR-Rust – A C Compiler in Rust  

This project implements the full compiler described in Raymond Rhine’s *Writing a C Compiler*, written in Rust and built on macOS (both Intel & Apple Silicon). The compiler supports a working subset of the C language and produces native executables for your machine. The project includes all four core compiler stages:  
  1. **Lexing** – tokenising the input source.  
  2. **Parsing** – building an Abstract Syntax Tree (AST).  
  3. **Code Generation** – emitting x86-64 (macOS) assembly.  
  4. **Assembly & Linking** – producing a runnable native executable.  

## Supported Subset of C  

This compiler currently supports the following features of C (subset as implemented):  
- Simple integer type (`int`) and integer return values.  
- Function definitions and calls returning `int`.  
- Arithmetic expressions (`+`, `-`, `*`, `/`, `%`).  
- Unary operators (once enabled) such as unary minus and logical/bitwise not.  
- Control-flow statements: `if`, `else`, `while`, `return`.  
- Local variables (in functions) and basic scoping.  
- No full standard library support; omitted features include pointers, arrays, structs/unions, floating-point, variadic functions, complex type casts, and advanced built-in types.  

## Command-Line Interface (CLI)  

The compiler exposes the following command-line interface:

```bash
Usage: ccr [OPTIONS] <PATH>

Arguments:
  <PATH>  Absolute or relative path to C source file

Options:
      --lex      Runs the lexer and exits
      --parse    Runs the parser and exits
      --codegen  Runs assembly generation and exits
  -h, --help     Print help
```

### Description of CLI Flags  

- `--lex` → run only the lexer and exit  
- `--parse` → run only the parser and exit  
- `--codegen` → run only up to code generation (no linking)  
- If none of those flags are provided, the full pipeline (lex → parse → codegen → link) runs.  
- `<path>` is required: the path (absolute or relative) to your C source file.

### Example Usage  

```bash
cargo run -- path/to/my_program.c
cargo run -- --lex path/to/my_program.c
cargo run -- --parse path/to/my_program.c
cargo run -- --codegen path/to/my_program.c
```

## Local Installation (macOS)  

**Prerequisites**:
- Rust toolchain installed (via `rustup`)  
- `cargo` (comes with Rust)  
- Unix-style linking tools (installed by default on macOS)  
