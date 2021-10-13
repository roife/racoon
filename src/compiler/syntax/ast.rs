use std::rc::Rc;
use super::{
    span::{Span}
};

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<DeclStmt>,
    pub funcs: Vec<FuncStmt>,
}

#[derive(Debug, Clone)]
pub struct FuncStmt {
    pub name: Ident,
    pub params: Vec<FuncParam>,
    pub ret_type: TypeDef,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub is_const: bool,
    pub name: Ident,
    pub ty: TypeDef,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(BlockStmt),
    Expr(Expr),
    Empty(Span),
    Decl(DeclStmt),
    If(IfStmt),
    While(WhileStmt),
    Break(Span),
    Continue(Span),
    Return(ReturnStmt),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Block(i) => i.span,
            Stmt::While(i) => i.span,
            Stmt::If(i) => i.span,
            Stmt::Expr(i) => i.span(),
            Stmt::Decl(i) => i.span(),
            Stmt::Return(i) => i.span,
            Stmt::Break(s) => *s,
            Stmt::Continue(s) => *s,
            Stmt::Empty(s) => *s,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclStmt {
    ConstDecl(ConstDeclStmt),
    VarDecl(VarDeclStmt)
}

impl DeclStmt {
    pub fn span(&self) -> Span {
        match self {
            DeclStmt::ConstDecl(x) => x.span,
            DeclStmt::VarDecl(x) => x.span
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstDeclStmt {
    pub name: Ident,
    pub ty: TypeDef,
    pub val: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub name: Ident,
    pub ty: TypeDef,
    pub val: Option<Rc<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub val: Option<Rc<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub span: Span,
    pub name: String,
    pub params: Option<Vec<TypeDef>>,
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub span: Span,
    pub cond: Rc<Expr>,
    pub body: Rc<BlockStmt>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub span: Span,
    pub cond: Rc<Expr>,
    pub if_block: Rc<BlockStmt>,
    pub else_block: IfElseBlock,
}

#[derive(Debug, Clone)]
pub enum IfElseBlock {
    None,
    If(Rc<IfStmt>),
    Block(Rc<BlockStmt>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Assign(AssignExpr),
    As(AsExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(x) => x.span,
            Expr::Assign(x) => x.span,
            Expr::As(x) => x.span,
            Expr::Literal(x) => x.span,
            Expr::Unary(x) => x.span,
            Expr::Binary(x) => x.span,
            Expr::Call(x) => x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub span: Span,
    pub val: LiteralVal,
}

#[derive(Debug, Clone)]
pub enum LiteralVal {
    Integer(u64),
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub span: Span,
    pub op: UnaryOp,
    pub expr: Rc<Expr>,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub span: Span,
    pub allow_assign_const: bool,
    pub lhs: Rc<Expr>,
    pub rhs: Rc<Expr>,
}

#[derive(Debug, Clone)]
pub struct AsExpr {
    pub span: Span,
    pub val: Rc<Expr>,
    pub ty: TypeDef,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub span: Span,
    pub op: BinaryOp,
    pub lhs: Rc<Expr>,
    pub rhs: Rc<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub span: Span,
    pub func: Ident,
    pub params: Vec<Expr>,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Not, Neg, Pos,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Gt, Lt, Ge, Le, Eq, Ne,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}
