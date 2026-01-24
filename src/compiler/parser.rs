use super::generate_tag;
use super::tokenizer::Token;

fn generate_label() -> String {
    generate_tag("label")
}

enum UnaryOp {
    Decr,
    Incr,
    Neg,
    Not,
    Compl,
}

enum BinOp {
    Add,
    Div,
    Sub,
    Mul,
    Rem,
    BitOr,
    BitAnd,
    BitXor,
    LShift,
    RShift,
    And,
    Or,
    EqEq,
    NotEq,
    LT,
    GT,
    LTE,
    GTE,
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    ProdAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
}

enum Factor {
    Int(u64),
    Ident(String),
    Expr(Box<Expr>),
    PreUnary(UnaryOp, Box<Self>),
    PosUnary(UnaryOp, Box<Self>),
}

enum Expr {
    Fac(Factor),
    Cond(Box<Self>, Box<Self>, Box<Self>),
    Bin(Box<Self>, BinOp, Box<Self>),
}

enum Decl {
    FnDecl(FnDecl),
    VarDecl(VarDecl),
}

struct VarDecl {
    name: String,
    expr: Option<Expr>,
}

struct FnDecl {
    name: String,
    params: Vec<String>,
    body: Block,
}

enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

struct Block {
    items: Vec<BlockItem>,
}

enum ForInit {
    Decl(Decl),
    Expr(Option<Expr>),
}

struct If {
    cond: Expr,
    body: Box<Self>,
    otherwise: Option<Box<Self>>,
}

struct Label {
    name: String,
    body: Box<Stmt>,
}

struct While {
    id: String,
    cond: Expr,
    body: Box<Stmt>,
}

enum Case {
    Int(u64, String),
    Default(String),
}

struct Switch {
    id: String,
    value: Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

struct Clause {
    parent: String,
    value: Expr,
    body: Box<Stmt>,
}

struct Default {
    parent: String,
    body: Box<Stmt>,
}

struct For {
    id: String,
    init: ForInit,
    cond: Option<Expr>,
    post: Option<Expr>,
    body: Box<Stmt>,
}

enum Stmt {
    Null,
    Return(Expr),
    Expr(Expr),
    If(If),
    Goto(String),
    Label(Label),
    Comp(Block),
    Break(String),
    Continue(String),
    While(While),
    DoWhile(While),
    Switch(Switch),
    Case(Clause),
    Default(Default),
    For(For),
}

struct Program(Vec<FnDecl>);

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    todo!()
}
