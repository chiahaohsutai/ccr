use super::{blocks, decls, exprs};

enum ForInit {
    Decl(decls::Decl),
    Expr(Option<exprs::Expr>),
}

struct If {
    cond: exprs::Expr,
    body: Box<Self>,
    otherwise: Option<Box<Self>>,
}

struct Label {
    name: String,
    body: Box<Stmt>,
}

struct While {
    id: String,
    cond: exprs::Expr,
    body: Box<Stmt>,
}

enum Case {
    Int(u64, String),
    Default(String),
}

struct Switch {
    id: String,
    value: exprs::Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

struct Clause {
    parent: String,
    value: exprs::Expr,
    body: Box<Stmt>,
}

struct Default {
    parent: String,
    body: Box<Stmt>,
}

struct For {
    id: String,
    init: ForInit,
    cond: Option<exprs::Expr>,
    post: Option<exprs::Expr>,
    body: Box<Stmt>,
}

pub enum Stmt {
    Null,
    Return(exprs::Expr),
    Expr(exprs::Expr),
    If(If),
    Goto(String),
    Label(Label),
    Comp(blocks::Block),
    Break(String),
    Continue(String),
    While(While),
    DoWhile(While),
    Switch(Switch),
    Case(Clause),
    Default(Default),
    For(For),
}
