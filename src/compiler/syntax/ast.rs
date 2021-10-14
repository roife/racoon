use std::rc::Rc;
use super::{
    span::{Span}
};

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<DeclStmt>,
    //pub funcs: Vec<FuncDef>,
}

#[derive(Debug, Clone)]
pub enum DeclStmt {
    Const(ConstDecl),
    Var(VarDecl)
}

// impl DeclStmt {
//     pub fn span(&self) -> Span {
//         match self {
//             DeclStmt::Const(x) => x.span,
//             DeclStmt::Var(x) => x.span
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct ConstDecl {
    // pub name: Ident,
    // pub ty: TypeDef,
    // pub val: Rc<Expr>,
    // pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    // pub name: Ident,
    // pub ty: TypeDef,
    // pub val: Option<Rc<Expr>>,
    // pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}
