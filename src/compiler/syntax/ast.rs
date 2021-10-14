use std::rc::Rc;
use super::{
    span::{Span},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<DeclStmt>,
    pub funcs: Vec<FuncStmt>
}

#[derive(Debug, Clone)]
pub struct DeclStmt {
    pub is_const: bool,
    pub name: Ident,
    pub ty: TyDef,
    pub val: Option<Rc<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncStmt {
    pub name: Ident,
    pub params: Vec<FuncParam>,
    pub ret_ty: TyDef,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: Ident,
    pub ty: TyDef,
}

#[derive(Debug, Clone)]
pub struct TyDef {
    // todo
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expr {

}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {

}