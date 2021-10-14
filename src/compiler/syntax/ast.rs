use std::rc::Rc;
use super::{
    span::{Span},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub funcs: Vec<Func>
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub is_const: bool,
    pub name: Ident,
    pub ty: TypeDef,
    pub init_val: Option<Rc<InitVal>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum InitVal {
    Expr(Expr),
    InitVal(Vec<Rc<InitVal>>)
}

impl InitVal {
    pub fn span(&self) -> Span {
        match self {
            InitVal::Expr(expr) => expr.span.clone(),
            InitVal::InitVal(vals) => Span {
                start: vals.first().unwrap().span().start,
                end: vals.last().unwrap().span().end
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: Ident,
    pub params: Vec<FuncParam>,
    pub ret_ty: TypeDef,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: Ident,
    pub dims: Option<Vec<Rc<Expr>>>,
    pub ty: TypeDef,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub block_items: Vec<BlockItem>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

impl BlockItem {
    pub fn span(&mut self) -> Span {
        match self {
            BlockItem::Stmt(v) => v.span(),
            BlockItem::Decl(v) => v.span
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    // todo: assignment
    Expr(Expr),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    Break(Span),
    Continue(Span),
    Return(ReturnStmt),
    Empty(Span),
}

impl Stmt {
    pub fn span(&mut self) -> Span {
        match self {
            Stmt::Expr(v) => v.span,
            Stmt::Block(v) => v.span,
            Stmt::If(v) => v.span,
            Stmt::While(v) => v.span,
            Stmt::Break(span) => *span,
            Stmt::Continue(span) => *span,
            Stmt::Return(v) => v.span,
            Stmt::Empty(span) => *span
        }
    }
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
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub name: Ident,
    pub dims: Option<Vec<Rc<Expr>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub ty_name: Ident,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}