use crate::compiler::ir::value::ty::IrTy;
use crate::compiler::span::Span;
use crate::compiler::syntax::ast::*;
use crate::compiler::syntax::visitor::AstVisitorMut;

pub struct TypeChecker {
}

impl AstVisitorMut for TypeChecker {
    type ProgramResult = ();
    type FuncResult = ();
    type StmtResult = ();
    type ExprResult = ();
    type LExprResult = ();
    type TyResult = ();

    fn visit_program(&mut self, program: &mut Program) -> Self::ProgramResult {
        todo!()
    }

    fn visit_decl(&mut self, decl: &mut Decl) -> Self::StmtResult {
        todo!()
    }

    fn visit_sub_decl(&mut self, sub_decl: &mut SubDecl, ty: IrTy) -> Self::StmtResult {
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

    fn visit_expr_stmt(&mut self, stmt: &mut Expr) -> Self::StmtResult {
        todo!()
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult {
        todo!()
    }

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult {
        todo!()
    }

    fn visit_return_stmt(&mut self, stmt: &mut ReturnStmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult {
        todo!()
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
        todo!()
    }
}