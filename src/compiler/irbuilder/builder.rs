use std::borrow::Borrow;

use crate::compiler::ir::{
    arena::{BBId, FuncId},
    value::{module::Module, ty::Ty, value::Operand},
};
use crate::compiler::ir::value::constant::Constant;
use crate::compiler::ir::value::inst::{BinaryInst, BinaryInstOp, CallInst, InstKind};
use crate::compiler::ir::value::ty::FuncTy;
use crate::compiler::span::Span;
use crate::compiler::syntax::{
    ast::*,
    visitor::AstVisitor,
};

use super::{
    context::{Context, NameId, ScopeBuilder},
    err::Error,
};

pub struct IrBuilder<'a> {
    ast_program: Program,
    ir_module: Module,
    ctx: Context<'a>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(ast_program: Program) -> IrBuilder<'a> {
        IrBuilder {
            ast_program,
            ir_module: todo!(),
            ctx: todo!(),
        }
    }
}

impl<'a> AstVisitor for IrBuilder<'a> {
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
        self.ctx.scope_builder.push_scope();
        let x = stmt.block_items.iter().try_for_each(|sub_stmt| match sub_stmt {
            BlockItem::Stmt(x) => self.visit_stmt(x),
            BlockItem::Decl(x) => self.visit_decl(x),
        })?;
        self.ctx.scope_builder.pop_scope();
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult {
        match stmt {
            Stmt::Expr(x) => todo!(),
            Stmt::Block(x) => self.visit_block_stmt(x),
            Stmt::If(x) => self.visit_if_stmt(x),
            Stmt::While(x) => self.visit_while_stmt(x),
            Stmt::Break(x) => self.visit_break_stmt(*x),
            Stmt::Continue(x) => self.visit_continue_stmt(*x),
            Stmt::Return(x) => self.visit_return_stmt(x),
            Stmt::Empty(x) => self.visit_empty_stmt(*x),
        }
    }

    fn visit_expr_stmt(&mut self, stmt: &Expr) -> Self::StmtResult {
        self.visit_expr(stmt);
        Ok(())
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
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprResult {
        // match expr {
        //     Expr::LVal(lvalue) => self.visit_lexpr(expr),
        //     Expr::Assign(x) => self.visit_assign_expr(x),
        //     Expr::Literal(x) => self.visit_literal_expr(x),
        //     Expr::Unary(x) => self.visit_unary_expr(x),
        //     Expr::Binary(x) => self.visit_binary_expr(x),
        //     Expr::Call(x) => self.visit_call_expr(x),
        // }
        todo!()
    }

    fn visit_lexpr(&mut self, expr: &Expr) -> Self::LExprResult {
        todo!()
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::ExprResult {
        let constant = match expr.kind {
            LiteralKind::Integer(i) => Operand::Constant(Constant::Int(i))
        };
        Ok(constant)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::ExprResult {
        let val = self.visit_expr(&expr.expr)?;
        let inst = match expr.op {
            UnaryOp::Neg => {
                let binary_inst = BinaryInst {
                    op: BinaryInstOp::Sub,
                    left: Operand::from(0),
                    right: val,
                };
                todo!()
            }
            UnaryOp::Pos => val,
            UnaryOp::Not => todo!()
        };
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult {
        let left = self.visit_expr(&expr.lhs)?;
        let right = self.visit_expr(&expr.rhs)?;
        let op = expr.op.to_binary_inst_kind();

        let binary_inst = BinaryInst { op, left, right };
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult {
        let func = self.ctx.scope_builder.find_name_rec(&expr.func.name)
            .ok_or_else(|| Error::UnknownName(expr.func.name.clone()))?
            .as_func()
            .ok_or_else(|| Error::ExpectedFunction(expr.func.name.clone()))?
            .clone();
        let args = expr.args.iter()
            .map(|x| self.visit_expr(&x))
            .collect::<Result<Vec<_>, _>>()?;

        let call_inst = CallInst { func, args };
        let ty: Ty = Ty::Void;
        let id = self.ctx.inst_new_at_end(InstKind::Call(call_inst), ty);
        Ok(id.into())
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
