use super::{decls, stmts};

enum BlockItem {
    Stmt(stmts::Stmt),
    Decl(decls::Decl),
}

pub struct Block {
    items: Vec<BlockItem>,
}
