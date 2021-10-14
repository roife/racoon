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
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    // todo: assignment
    Expr(Expr),
    // todo: Decl(DeclStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    Break(Span),
    Continue(Span),
    Return(ReturnStmt),
    // todo: Empty(Span),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Rc<Expr>,
    pub then_block: Rc<BlockStmt>,
    pub else_block: Option<Rc<BlockStmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Rc<Expr>,
    pub body: Rc<BlockStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub val: Option<Rc<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expr {

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