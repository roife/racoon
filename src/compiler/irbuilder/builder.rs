use std::borrow::Borrow;

use crate::compiler::ir::{
    arena::{BBId, FuncId},
    value::{
        constant::Constant,
        inst::*,
        module::Module,
        ty::IrTy,
        value::Operand,
    },
};
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
    type TyResult = Result<IrTy, Error>;

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
        let cond = self.visit_expr(&stmt.cond)?;

        // cond blk
        let cond_bb = self.ctx.build_bb_after_cur();
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
        let val = stmt.val.as_ref()
            .and_then(|val| Some(self.visit_expr(&val)))
            .map_or(Ok(None), |r| r.map(Some))?;

        let inst = RetInst { val };
        self.ctx.build_inst_at_end(InstKind::ReturnInst(inst), IrTy::Void);

        let nxt_bb = self.ctx.build_bb_after_cur(); // todo? maybe a bug
        self.ctx.set_cur_bb(nxt_bb);
        Ok(())
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
        let mut val = self.visit_expr(&expr.expr)?;
        match expr.op {
            UnaryOp::Neg => {
                let inst = BinaryInst {
                    op: BinaryInstOp::Sub,
                    left: Operand::from(0),
                    right: val,
                };
                let id = self.ctx.build_inst_at_end(InstKind::Binary(inst), expr.ty.into());
                Ok(id.into())
            }
            UnaryOp::Pos => Ok(val),
            UnaryOp::Not => {
                let sub_expr_ty = expr.expr.ty();
                if let AstTy::Bool = sub_expr_ty {
                    let zext_inst = ZExtInst {
                        ori_val: val,
                        target_ty: IrTy::int(),
                    };
                    let id = self.ctx.build_inst_at_end(InstKind::ZExt(zext_inst), IrTy::int());
                    val = Operand::from(id);
                }
                let inst = BinaryInst {
                    op: BinaryInstOp::Ne,
                    left: val,
                    right: Operand::from(0),
                };
                let id = self.ctx.build_inst_at_end(InstKind::Binary(inst), expr.ty.into());
                Ok(id.into())
            }
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult {
        let left = self.visit_expr(&expr.lhs)?;
        let right = self.visit_expr(&expr.rhs)?;
        let op = expr.op.to_binary_inst_kind();

        let inst = BinaryInst { op, left, right };
        let id = self.ctx.build_inst_at_end(InstKind::Binary(inst), expr.ty.into());
        Ok(id.into())
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

        let ret_ty = self.ctx.get_func_ret_ty();
        let inst = CallInst { func, args };
        let id = self.ctx.build_inst_at_end(InstKind::Call(inst), ret_ty);
        Ok(id.into())
    }

    fn visit_ty(&mut self, ty_def: &TypeDef) -> Self::TyResult {
        let ty = match &ty_def.ty_kind {
            TyKind::Primitive(prim_ty) => match prim_ty {
                PrimitiveTy::Integer => IrTy::Int(32)
            }
            TyKind::Void => IrTy::Void
        };
        Ok(ty)
    }
}

impl From<AstTy> for IrTy {
    fn from(ast_ty: AstTy) -> Self {
        match ast_ty {
            AstTy::Void => IrTy::Void,
            AstTy::Int => IrTy::Int(32),
            AstTy::Bool => IrTy::Int(1),
            AstTy::Unknown => unreachable!()
        }
    }
}