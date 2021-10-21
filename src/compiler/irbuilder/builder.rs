use std::borrow::Borrow;
use crate::compiler::ir::{
    arena::{BBId, FuncId},
    value::{ty::Ty, value::Operand}
};
use crate::compiler::irbuilder::context::NameId;
use crate::compiler::span::Span;
use super::{context::{ScopeBuilder, Context}, err::Error};
use crate::compiler::syntax::{ast::*, visitor::AstVisitor};

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
    type ExprResult = Result<Operand, Error>;
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

    fn visit_func_param(&mut self, param: &FuncParam) -> Self::StmtResult {
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
        let func_ty: &Box<Ty> = self.ctx.scope_builder
            .find_name_rec(&expr.func.name)
            .ok_or_else(|| Error::UnknownName(expr.func.name.clone()))?
            .ty.as_func()
            .ok_or_else(|| Error::ExpectedFunction(expr.func.name.clone()))?;

        let mut params = vec![];
        let mut types = vec![];
        for sub_expr in &expr.params {
            let val = self.visit_expr(&sub_expr)?;
            params.push(val);
            types.push(self.ctx.cur_func);
        }

        // if types.len() != func_ty.params.len() {
        //     return Err(Error::WrongParamLength {
        //         expected: func_ty.params.len(),
        //         found: types.len(),
        //     });
        // }
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
