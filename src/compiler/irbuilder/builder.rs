use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use crate::compiler::ir::inst::{BinaryInst, CallInst};
use crate::compiler::ir::value::BasicBlock;
use crate::compiler::ptr::{MutRc, MutWeak};

use super::{
    context::ScopeBuilder,
    err::Error,
    super::ir::value::{self as ir, Ty, Use, Value},
    super::span::Span,
    super::syntax::{ast::*, visitor::AstVisitor},
};

struct Context {
    scope_builder: ScopeBuilder,
    cur_func: MutWeak<ir::Func>,
    cur_bb: MutWeak<BasicBlock>,
}

pub struct IrBuilder {
    ast_program: Program,
    ctx: Context,
}

impl IrBuilder {
    pub fn new(ast_program: Program) -> IrBuilder {
        IrBuilder {
            ast_program,
            ctx: todo!(),
        }
    }
}

impl AstVisitor for IrBuilder {
    type ProgramResult = ();
    type FuncResult = Result<(), Error>;
    type StmtResult = Result<(), Error>;
    type ExprResult = Result<(MutWeak<Value>, Ty), Error>;
    type LExprResult = ();
    type TyResult = Result<Ty, Error>;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult {
        todo!()
    }

    fn visit_decl(&mut self, decl: &Decl) -> Self::StmtResult {
        todo!()
    }

    fn visit_func(&mut self, func: &Func) -> Self::FuncResult {
        todo!()
    }

    fn visit_func_param(&mut self, _param: &FuncParam) -> Self::StmtResult {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult {
        todo!()
    }

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult {
        todo!()
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult {
        todo!()
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        todo!()
    }

    fn visit_lexpr(&mut self, _expr: &Expr) -> Self::LExprResult {
        todo!()
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_ty(&mut self, ty_def: &TypeDef) -> Self::TyResult {
        let ty = match &ty_def.ty_kind {
            TyKind::Primitive(prim_ty) => match prim_ty {
                PrimitiveTy::Integer => Ty::Int(32)
            }
            TyKind::Void => Ty::Void
        };
        Ok(ty)
    }
}

fn assert_type_eq(lhs: &Ty, rhs: &Ty) -> Result<(), Error> {
    todo!()
}
