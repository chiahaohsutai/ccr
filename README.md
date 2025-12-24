# Sandglass

Sandglass is a C compiler implemented in Rust, based on the curriculum from Nora Sandler’s [Writing a C Compiler: Build a Real Programming Language from Scratch](https://nostarch.com/writing-c-compiler).

This project presents a full implementation of the compiler described in the text. It supports a subset of the C language, including arithmetic expressions, bitwise operations, and control-flow statements. The compiler is structured into five compilation stages:

1. **Lexing (Tokenization)** – Converts the input source code into a stream of tokens.
2. **Parsing** – Builds an abstract syntax tree (AST) using recursive descent with precedence climbing.
3. **Tacky (Three-Address Code)** – Linearizes the AST into a sequence of simple instructions using three-address code (TAC).
4. **Code Generation** – Translates TAC instructions into target assembly code.
5. **Linking** – Assembles and links the generated assembly into a native executable.

## Command-Line Interface (CLI)

You can interact with the compiler through a command-line interface (CLI) to compile C programs into binaries or stop execution at specific stages. Note that the compiler targets the x86_64 architecture; therefore, your shell instance must be running on an x86_64 host to successfully compile and execute the generated binaries.

```bash
# Display available CLI options and flags
cargo run -- --help

# Compile the C program normally (runs all compilation stages)
cargo run -- path/to/my_program.c

# Run the compiler and stop after the lexing stage
cargo run -- --lex path/to/my_program.c

# Run the compiler and stop after the parsing stage (AST generation)
cargo run -- --parse path/to/my_program.c

# Run the compiler and stop after code generation (emit assembly)
cargo run -- --codegen path/to/my_program.c
```

## Local Installation

**Prerequisites**:

To build and run this project, ensure the following command-line tools are installed:

* The Rust toolchain, installed via `rustup`, with `cargo`
* A system C compiler (`gcc` or `clang`, accessible as `gcc`)

```bash
# Build the project
cargo build

# If on Apple Silicon Mac, run the shell in Intel (x86_64) mode:
arch -x86_64 zsh

# Run the compiler
cargo run -- path/to/my_program.c
```
