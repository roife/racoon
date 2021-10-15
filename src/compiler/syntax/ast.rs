use std::rc::Rc;

use super::{
    span::Span,
    token::TokenType,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub funcs: Vec<Func>,
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub is_const: bool,
    pub ty: TypeDef,
    pub sub_decls: Vec<SubDecl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SubDecl {
    pub name: Ident,
    pub dims: Option<Dim>,
    pub init_val: Option<Rc<InitVal>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum InitVal {
    Expr(Expr),
    ArrayVal(Vec<Rc<InitVal>>),
}

impl InitVal {
    pub fn span(&self) -> Span {
        match self {
            InitVal::Expr(expr) => expr.span().clone(),
            InitVal::ArrayVal(vals) => Span {
                start: vals.first().unwrap().span().start,
                end: vals.last().unwrap().span().end,
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
    pub dims: Option<Dim>,
    pub ty: TypeDef,
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
    pub fn span(&mut self) -> Span {
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
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub lhs: Rc<Expr>,
    pub rhs: Rc<Expr>,
    pub allow_assign_const: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Integer(i32),
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Rc<Expr>,
    pub rhs: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Ident,
    pub params: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub name: Ident,
    pub dims: Option<Dim>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Dim {
    pub dims: Vec<Rc<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub ty_kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyKind {
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
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
}

impl TokenType {
    pub fn is_binary_op(&self) -> bool {
        use super::token::TokenType::*;
        matches!(
            self,
            Assign | Plus | Minus | Mul | Div | Eq | Ne | Lt | Gt | Le | Ge
        )
    }

    pub fn prec(&self) -> u32 {
        use super::token::TokenType::*;
        match self {
            Plus | Minus => 10,
            Mul | Div => 20,
            Assign => 1,
            Eq | Ne | Lt | Gt | Le | Ge => 2,
            _ => unreachable!(),
        }
    }

    pub fn is_left_assoc(&self) -> bool {
        use super::token::TokenType::*;
        match self {
            Plus | Minus | Mul | Div | Eq | Ne | Lt | Gt | Le | Ge => true,
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
            TokenType::Eq => Some(BinaryOp::Eq),
            TokenType::Ne => Some(BinaryOp::Ne),
            TokenType::Lt => Some(BinaryOp::Lt),
            TokenType::Gt => Some(BinaryOp::Gt),
            TokenType::Le => Some(BinaryOp::Le),
            TokenType::Ge => Some(BinaryOp::Ge),
            _ => None,
        }
    }

    pub fn to_ty_kind(&self) -> Option<TyKind> {
        match self {
            TokenType::IntTy => Some(TyKind::Primitive(PrimitiveTy::Integer)),
            TokenType::VoidTy => Some(TyKind::Void),
            _ => None,
        }
    }

    pub fn is_ty(&self) -> bool {
        use super::token::TokenType::*;
        matches!(
            self,
            IntTy | VoidTy
        )
    }
}
