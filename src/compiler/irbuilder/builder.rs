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
    context::{Context, NameId, ScopeBuilder},
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
    type ExprResult = Result<(Operand, Ty), Error>;
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
        let constant = match expr.kind {
            LiteralKind::Integer(i) => (Operand::Constant(Constant::Int(i)), Ty::int())
        };
        Ok(constant)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::ExprResult {
        let (val, ty) = self.visit_expr(&expr.expr)?;
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult {
        let (lhs_val, lhs_ty) = self.visit_expr(&expr.lhs)?;
        let (rhs_val, rhs_ty) = self.visit_expr(&expr.rhs)?;

        assert_type_eq(&lhs_ty, &rhs_ty)?;

        if matches!(expr.op, BinaryOp::And | BinaryOp::Or) {
            todo!()
        }

        let (op, ty) = match expr.op {
            BinaryOp::Add => (BinaryInstOp::Add, lhs_ty),
            BinaryOp::Sub => (BinaryInstOp::Sub, lhs_ty),
            BinaryOp::Mul => (BinaryInstOp::Mul, lhs_ty),
            BinaryOp::Div => (BinaryInstOp::Div, lhs_ty),
            BinaryOp::Mod => (BinaryInstOp::Mod, lhs_ty),
            BinaryOp::Gt => (BinaryInstOp::Gt, Ty::bool()),
            BinaryOp::Lt => (BinaryInstOp::Lt, Ty::bool()),
            BinaryOp::Ge => (BinaryInstOp::Ge, Ty::bool()),
            BinaryOp::Le => (BinaryInstOp::Le, Ty::bool()),
            BinaryOp::Eq => (BinaryInstOp::Eq, Ty::bool()),
            BinaryOp::Ne => (BinaryInstOp::Ne, Ty::bool()),
            BinaryOp::And => (BinaryInstOp::And, Ty::bool()),
            BinaryOp::Or => (BinaryInstOp::Or, Ty::bool()),
        };

        let binary_inst = BinaryInst {
            op,
            left: lhs_val,
            right: rhs_val,
        };
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult {
        let func = self.ctx.scope_builder.find_name_rec(&expr.func.name)
            .ok_or_else(|| Error::UnknownName(expr.func.name.clone()))?;
        let func_ty = func.ty.as_func()
            .ok_or_else(|| Error::ExpectedFunction(expr.func.name.clone()))?
            .clone();
        let func_id = func.id.as_func().unwrap().clone();

        let mut args = vec![];
        let mut arg_tys = vec![];
        for sub_expr in &expr.args {
            let (val, ty) = self.visit_expr(&sub_expr)?;
            args.push(val);
            arg_tys.push(ty);
        }

        // type check
        if arg_tys.len() != func_ty.params_ty.len() {
            return Err(Error::WrongParamLength {
                expected: func_ty.params_ty.len(),
                found: arg_tys.len(),
            });
        }

        for (arg_ty, param_ty) in arg_tys.iter().zip(func_ty.params_ty.iter()) {
            assert_type_eq(arg_ty, param_ty)?;
        }

        // insert inst
        let call_inst = CallInst {
            func: func_id,
            args,
        };
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
