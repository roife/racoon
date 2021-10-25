use std::borrow::BorrowMut;

use crate::compiler::ir::value::ty::IrTy;
use crate::compiler::irbuilder::context::Context;
use crate::compiler::irbuilder::err::SemanticError;
use crate::compiler::span::Span;
use crate::compiler::syntax::ast::*;
use crate::compiler::syntax::visitor::AstVisitorMut;

use super::err::SemanticError::*;

macro_rules! expect_type {
    ($self:expr, $pat:pat) => {
        let ty = $self.clone();
        if matches!(ty, $pat) {
            Err(TypeMismatch {
                expected: String::from(stringify!($pat)),
                found: $self
            })
        } else {
            Ok(ty)
        }
    };
}

pub struct TypeChecker {
    pub ctx: Context,
}

impl AstVisitorMut for TypeChecker {
    type ProgramResult = Result<(), SemanticError>;
    type ConstInitValResult = Result<IrTy, SemanticError>;
    type FuncResult = Result<(), SemanticError>;
    type StmtResult = Result<(), SemanticError>;
    type ExprResult = Result<IrTy, SemanticError>;
    type LExprResult = Result<IrTy, SemanticError>;
    type TyResult = Result<IrTy, SemanticError>;

    fn visit_program(&mut self, program: &mut Program) -> Self::ProgramResult {
        program.program_items.iter_mut().try_for_each(|item| {
            match item {
                ProgramItem::Decl(x) => self.visit_global_decl(x),
                ProgramItem::Func(x) => self.visit_func(x),
            }
        })
    }

    fn visit_global_decl(&mut self, decl: &mut Decl) -> Self::StmtResult {
        todo!()
    }

    fn visit_func(&mut self, func: &mut AstFunc) -> Self::FuncResult {
        todo!()
    }

    fn visit_func_param(&mut self, _param: &mut FuncParam) -> Self::StmtResult {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: &mut BlockStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_decl_stmt(&mut self, decl: &mut Decl) -> Self::StmtResult {
        todo!()
    }

    fn visit_expr_stmt(&mut self, stmt: &mut Expr) -> Self::StmtResult {
        todo!()
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStmt) -> Self::StmtResult {
        let cond = self.visit_expr(&mut stmt.cond)?;
        expect_type!(cond, IrTy::Int(_));
        self.visit_stmt(&mut stmt.then_block)?;
        if let Some(else_blk) = &mut stmt.else_block {
            self.visit_stmt(else_blk)?;
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) -> Self::StmtResult {
        let cond = self.visit_expr(&mut stmt.cond)?;
        self.visit_stmt(&mut stmt.body)?;
        Ok(())
    }

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &mut ReturnStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult {
        todo!()
    }

    fn visit_lexpr(&mut self, _expr: &mut Expr) -> Self::LExprResult {
        todo!()
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_literal_expr(&mut self, _expr: &mut LiteralExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &mut CallExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_ty(&mut self, ty_def: &mut TypeDef) -> Self::TyResult {
        let ty = match &ty_def.ty_ident {
            TyIdent::Primitive(prim_ty) => match prim_ty {
                PrimitiveTy::Integer => IrTy::Int(32)
            }
            TyIdent::Void => IrTy::Void
        };
        Ok(ty)
    }
}