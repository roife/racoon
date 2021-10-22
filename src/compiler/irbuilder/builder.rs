use std::borrow::Borrow;

use crate::compiler::ir::{
    arena::{BBId, FuncId},
    value::{module::Module, ty::Ty, value::Operand},
};
use crate::compiler::ir::value::constant::Constant;
use crate::compiler::ir::value::inst::{BinaryInst, BinaryInstOp, CallInst};
use crate::compiler::ir::value::ty::FuncTy;
use crate::compiler::span::Span;
use crate::compiler::syntax::{
    ast::*,
    visitor::AstVisitor,
};

use super::{
    context::{Context, Name, ScopeBuilder},
    err::Error,
};

pub struct IrBuilder {
    ast_program: Program,
    ir_module: Module,
    ctx: Context,
}

impl IrBuilder {
    pub fn new(ast_program: Program) -> IrBuilder {
        IrBuilder {
            ast_program,
            ir_module: todo!(),
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
        self.ctx.scope_builder.push_scope();
        let x = stmt.block_items.iter().try_for_each(|sub_stmt| match sub_stmt {
            BlockItem::Stmt(x) => self.visit_stmt(x),
            BlockItem::Decl(x) => self.visit_decl(x),
        })?;
        self.ctx.scope_builder.pop_scope();
        Ok(())
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
        // let val = self.visit_expr(&expr.expr)?;
        // let inst = match expr.op {
        //     UnaryOp::Neg => BinaryInst {
        //         op: BinaryInstOp::Sub,
        //         left: Operand::from(0),
        //         right: val,
        //     },
        //     UnaryOp::Pos => val,
        //     UnaryOp::Not => todo!()
        // };
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
