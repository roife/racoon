use std::rc::Rc;
use std::sync::Arc;
use enum_as_inner::EnumAsInner;

use crate::compiler::span::Span;

use super::token::TokenType;

#[derive(Debug, Clone)]
pub struct Program {
    pub program_items: Vec<ProgramItem>,
}

#[derive(Debug, Clone)]
pub enum ProgramItem {
    Decl(Decl),
    Func(AstFunc)
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub is_const: bool,
    pub ty_ident: TypeIdent,
    pub sub_decls: Vec<SubDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SubDecl {
    pub ident: Ident,
    pub subs: Option<Subs>,
    pub init_val: Option<InitVal>,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone)]
pub struct InitVal {
    pub ty: AstTy,
    pub kind: InitValKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum InitValKind {
    Expr(Expr),
    ArrayVal(Vec<InitVal>),
}

#[derive(Debug, Clone)]
pub struct AstFunc {
    pub ident: Ident,
    pub params: Vec<FuncParam>,
    pub ret_ty_ident: TypeIdent,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub ident: Ident,
    pub subs: Option<Subs>,
    pub ty_ident: TypeIdent,
    pub ty: AstTy,
    pub span: Span,
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
            BlockItem::Decl(v) => v.span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
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
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expr(v) => v.span(),
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
    pub cond: Box<Expr>,
    pub then_block: Box<Stmt>,
    pub else_block: Option<Box<Stmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Box<Expr>,
    pub body: Box<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub val: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Expr {
    LVal(LVal),
    Assign(AssignExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::LVal(x) => x.span,
            Expr::Assign(x) => x.span,
            Expr::Literal(x) => x.span,
            Expr::Unary(x) => x.span,
            Expr::Binary(x) => x.span,
            Expr::Call(x) => x.span,
        }
    }

    pub fn ty(&self) -> AstTy {
        match self {
            Expr::LVal(x) => x.ty.clone(),
            Expr::Assign(x) => x.ty.clone(),
            Expr::Literal(x) => x.ty.clone(),
            Expr::Unary(x) => x.ty.clone(),
            Expr::Binary(x) => x.ty.clone(),
            Expr::Call(x) => x.ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub allow_assign_const: bool,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub kind: LiteralKind,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum LiteralKind {
    Integer(i32),
    // Array(Vec<Option<Box<LiteralExpr>>>)
}

impl LiteralExpr {
    pub fn get_int(&self) -> Option<i32> {
        self.kind.as_integer().and_then(|x| Some(*x))
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub sub_expr: Box<Expr>,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: Ident,
    pub subs: Option<Subs>,
    pub span: Span,
    pub ty: AstTy,
}

#[derive(Debug, Clone)]
pub struct Subs {
    pub subs: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeIdent {
    pub ty_ident: TyIdent,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyIdent {
    Primitive(PrimitiveTy),
    Void,
}

#[derive(Debug, Clone)]
pub enum PrimitiveTy {
    Integer,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Neg,
    Pos,
    Not,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum AstTy {
    Unknown,
    Void,
    Int,
    Bool,
    Func { ret_ty: Box<AstTy>, param_tys: Vec<Box<AstTy>> }
}

impl TokenType {
    pub fn is_binary_op(&self) -> bool {
        use super::token::TokenType::*;
        matches!(
            self,
            Assign | Plus | Minus | Mul | Div | Mod | Eq | Ne | Lt | Gt | Le | Ge | Not | And | Or
        )
    }

    pub fn prec(&self) -> u32 {
        use super::token::TokenType::*;
        match self {
            Mul | Div => 20,
            Plus | Minus | Mod => 10,
            Lt | Gt | Le | Ge => 5,
            Eq | Ne => 4,
            And => 3,
            Or => 2,
            Assign => 1,
            _ => unreachable!(),
        }
    }

    pub fn is_left_assoc(&self) -> bool {
        use super::token::TokenType::*;
        match self {
            Plus | Minus | Mul | Div | Mod | Eq | Ne | Lt | Gt | Le | Ge | And | Or => true,
            Assign => false,
            _ => unreachable!(),
        }
    }

    pub fn to_binary_op(&self) -> Option<BinaryOp> {
        match self {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Sub),
            TokenType::Mul => Some(BinaryOp::Mul),
            TokenType::Div => Some(BinaryOp::Div),
            TokenType::Mod => Some(BinaryOp::Mod),
            TokenType::Eq => Some(BinaryOp::Eq),
            TokenType::Ne => Some(BinaryOp::Ne),
            TokenType::Lt => Some(BinaryOp::Lt),
            TokenType::Gt => Some(BinaryOp::Gt),
            TokenType::Le => Some(BinaryOp::Le),
            TokenType::Ge => Some(BinaryOp::Ge),
            TokenType::And => Some(BinaryOp::And),
            TokenType::Or => Some(BinaryOp::Or),
            _ => None,
        }
    }

    pub fn to_ty_ident(&self) -> Option<TyIdent> {
        match self {
            TokenType::IntTy => Some(TyIdent::Primitive(PrimitiveTy::Integer)),
            TokenType::VoidTy => Some(TyIdent::Void),
            _ => None,
        }
    }

    pub fn is_unary_op(&self) -> bool {
        use super::token::TokenType::*;
        matches!(
            self,
            Minus | Plus | Not
        )
    }

    pub fn to_unary_op(&self) -> Option<UnaryOp> {
        match self {
            TokenType::Plus => Some(UnaryOp::Pos),
            TokenType::Minus => Some(UnaryOp::Neg),
            TokenType::Not => Some(UnaryOp::Not),
            _ => None,
        }
    }

    pub fn is_ty(&self) -> bool {
        use super::token::TokenType::*;
        matches!(self, IntTy | VoidTy)
    }
}
