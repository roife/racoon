use super::ast::*;
use super::super::span::Span;

pub trait AstVisitor {
    type ProgramResult;
    type ConstInitValResult;
    type FuncResult;
    type StmtResult;
    type ExprResult;
    type LExprResult;
    type TyResult;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult;

    fn visit_const_init_val(&mut self, init_val: &InitVal) -> Self::ConstInitValResult;

    fn visit_global_decl(&mut self, decl: &Decl) -> Self::StmtResult;

    fn visit_func(&mut self, func: &AstFunc) -> Self::FuncResult;

    fn visit_func_param(&mut self, _param: &FuncParam) -> Self::StmtResult;

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::StmtResult;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult;

    fn visit_decl_stmt(&mut self, decl: &Decl) -> Self::StmtResult;

    fn visit_expr_stmt(&mut self, stmt: &Expr) -> Self::StmtResult;

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::StmtResult;

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::StmtResult;

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult;

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::StmtResult;

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult;

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult;

    fn visit_lexpr(&mut self, _expr: &Expr) -> Self::LExprResult;

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::ExprResult;

    fn visit_literal_expr(&mut self, _expr: &LiteralExpr) -> Self::ExprResult;

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::ExprResult;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult;

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult;

    fn visit_ty(&mut self, ty_def: &TypeIdent) -> Self::TyResult;
}

pub trait AstVisitorMut {
    type ProgramResult;
    type ConstInitValResult;
    type FuncResult;
    type StmtResult;
    type ExprResult;
    type LExprResult;
    type TyResult;

    fn visit_program(&mut self, program: &mut Program) -> Self::ProgramResult;

    fn visit_const_init_val(&mut self, init_val: &mut InitVal) -> Self::ConstInitValResult;

    fn visit_global_decl(&mut self, decl: &mut Decl) -> Self::StmtResult;

    fn visit_func(&mut self, func: &mut AstFunc) -> Self::FuncResult;

    fn visit_func_param(&mut self, _param: &mut FuncParam) -> Self::StmtResult;

    fn visit_block_stmt(&mut self, stmt: &mut BlockStmt) -> Self::StmtResult;

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult;

    fn visit_decl_stmt(&mut self, decl: &mut Decl) -> Self::StmtResult;

    fn visit_expr_stmt(&mut self, stmt: &mut Expr) -> Self::StmtResult;

    fn visit_if_stmt(&mut self, stmt: &mut IfStmt) -> Self::StmtResult;

    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) -> Self::StmtResult;

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult;

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult;

    fn visit_return_stmt(&mut self, stmt: &mut ReturnStmt) -> Self::StmtResult;

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult;

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult;

    fn visit_lexpr(&mut self, _expr: &mut Expr) -> Self::LExprResult;

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) -> Self::ExprResult;

    fn visit_literal_expr(&mut self, _expr: &mut LiteralExpr) -> Self::ExprResult;

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) -> Self::ExprResult;

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) -> Self::ExprResult;

    fn visit_call_expr(&mut self, expr: &mut CallExpr) -> Self::ExprResult;

    fn visit_ty(&mut self, ty_def: &mut TypeIdent) -> Self::TyResult;
}

