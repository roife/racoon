use crate::compiler::ir::{
    value::{
        constant::Constant,
        func::IrFunc,
        inst::*,
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
    context::{IrCtx, NameId},
    err::Error,
};

pub struct IrBuilder {
    ast_program: Program,
    ctx: IrCtx,
}

impl IrBuilder {
    pub fn new(ast_program: Program) -> IrBuilder {
        IrBuilder {
            ast_program,
            ctx: IrCtx::new()
        }
    }
}

impl AstVisitor for IrBuilder {
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
        let ty = self.visit_ty(&decl.ty)?;

        for sub_decl in &decl.sub_decls {
            self.visit_sub_decl(&sub_decl, ty.clone())?;
        }

        todo!()
    }

    fn visit_sub_decl(&mut self, sub_decl: &SubDecl, ty: IrTy) -> Self::StmtResult {
        // let inst = self.ctx.build_inst_end_of_cur(
        //     InstKind::Alloca(AllocaInst { alloca_ty: IrTy }),
        //     IrTy::ptr_of(IrTy)
        // );
        // self.ctx.scope_builder.insert(&sub_decl.name.name, NameId::Inst(inst))
        //     .ok_or(Error::DuplicateName(sub_decl.name.name.clone()))?;
        // Ok(())
        todo!()
    }

    fn visit_func(&mut self, ast_func: &AstFunc) -> Self::FuncResult {
        self.ctx.scope_builder.push_scope();

        let ret_ty = self.visit_ty(&ast_func.ret_ty)?;
        let func = self.ctx.build_func(IrFunc::new(&ast_func.func_name.name, ret_ty, false));
        self.ctx.scope_builder.insert(&ast_func.func_name.name, NameId::Func(func))
            .ok_or(Error::DuplicateName(ast_func.func_name.name.clone()))?;
        self.ctx.set_cur_func(func);

        let init_bb = self.ctx.build_bb();
        self.ctx.set_cur_bb(init_bb);
        ast_func.params.iter().try_for_each(|param| self.visit_func_param(param))?;
        self.visit_block_stmt(&ast_func.body)?;

        self.ctx.scope_builder.pop_scope();
        Ok(())
    }

    fn visit_func_param(&mut self, param: &FuncParam) -> Self::StmtResult {
        let ty = self.visit_ty(&param.ty)?;
        let inst = self.ctx.build_inst_end_of_cur(
            InstKind::Alloca(AllocaInst { alloca_ty: ty.clone() }),
            IrTy::ptr_of(ty.clone())
        );
        self.ctx.scope_builder.insert(&param.param_name.name, NameId::Inst(inst))
            .ok_or(Error::DuplicateName(param.param_name.name.clone()))?;
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::StmtResult {
        self.ctx.scope_builder.push_scope();
        stmt.block_items.iter().try_for_each(|sub_stmt| match sub_stmt {
            BlockItem::Stmt(x) => self.visit_stmt(x),
            BlockItem::Decl(x) => self.visit_decl(x),
        })?;
        self.ctx.scope_builder.pop_scope();
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtResult {
        match stmt {
            Stmt::Expr(x) => self.visit_expr_stmt(x),
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
        self.visit_expr(stmt)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::StmtResult {
        let cond = self.visit_expr(&stmt.cond)?;
        let old_bb = self.ctx.get_cur_bb_id();

        // then blk
        let then_bb = self.ctx.build_bb_after_cur();
        self.ctx.set_cur_bb(then_bb);
        self.visit_block_stmt(&stmt.then_block)?;
        let then_bb_end = self.ctx.get_cur_bb_id();

        // else bb
        let (else_bb, else_bb_end) = &stmt.else_block.as_ref()
            .map_or(Ok((None, None)), |else_blk| {
                let else_bb = self.ctx.build_bb_after_cur();
                self.ctx.set_cur_bb(else_bb);
                self.visit_block_stmt(&else_blk)?;
                Ok((Some(else_bb), Some(self.ctx.get_cur_bb_id())))
            })?;

        // nxt bb
        let nxt_bb = self.ctx.build_bb_after_cur();

        self.ctx.build_inst_end(
            InstKind::Branch(BranchInst::Br {
                cond,
                true_bb: then_bb,
                false_bb: else_bb.unwrap_or(nxt_bb),
            }),
            IrTy::Void,
            old_bb);
        self.ctx.build_inst_end(
            InstKind::Branch(BranchInst::Jump { nxt_bb }),
            IrTy::Void,
            then_bb_end);
        if let Some(else_bb_end) = else_bb_end {
            self.ctx.build_inst_end(
                InstKind::Branch(BranchInst::Jump { nxt_bb }),
                IrTy::Void,
                *else_bb_end);
        }

        self.ctx.set_cur_bb(nxt_bb);
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::StmtResult {
        let old_bb = self.ctx.get_cur_bb_id();

        // cond_bb
        let cond_bb = self.ctx.build_bb_after_cur();
        self.ctx.set_cur_bb(cond_bb);
        let cond = self.visit_expr(&stmt.cond)?;

        // loop_bb, nxt_bb
        let loop_bb = self.ctx.build_bb_after_cur();
        let nxt_bb = self.ctx.build_bb_after_cur();

        self.ctx.set_cur_bb(loop_bb);
        self.ctx.push_break_target(nxt_bb, cond_bb);
        self.visit_block_stmt(&stmt.body)?;
        self.ctx.pop_loop_target();
        let loop_end_bb = self.ctx.get_cur_bb_id();

        self.ctx.build_inst_end(
            InstKind::Branch(BranchInst::Jump { nxt_bb: cond_bb }),
            IrTy::Void,
            old_bb);

        self.ctx.build_inst_end(
            InstKind::Branch(BranchInst::Br {
                cond,
                true_bb: loop_bb,
                false_bb: nxt_bb,
            }),
            IrTy::Void,
            cond_bb);

        self.ctx.build_inst_end(
            InstKind::Branch(BranchInst::Jump { nxt_bb }),
            IrTy::Void,
            loop_end_bb);

        self.ctx.set_cur_bb(loop_end_bb);
        Ok(())
    }

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult {
        let break_target = self.ctx.get_break_target()
            .ok_or(Error::BreakOutsideLoop)?;
        self.ctx.build_inst_end_of_cur(
            InstKind::Branch(BranchInst::Jump { nxt_bb: break_target }),
            IrTy::Void);
        let nxt_bb = self.ctx.build_bb_after_cur();
        self.ctx.set_cur_bb(nxt_bb);
        Ok(())
    }

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult {
        let continue_target = self.ctx.get_continue_target()
            .ok_or(Error::ContinueOutsideLoop)?;
        self.ctx.build_inst_end_of_cur(
            InstKind::Branch(BranchInst::Jump { nxt_bb: continue_target }),
            IrTy::Void);
        let nxt_bb = self.ctx.build_bb_after_cur();
        self.ctx.set_cur_bb(nxt_bb);
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::StmtResult {
        let val = stmt.val.as_ref()
            .and_then(|val| Some(self.visit_expr(&val)))
            .map_or(Ok(None), |r| r.map(Some))?;

        let inst = RetInst { val };
        self.ctx.build_inst_end_of_cur(InstKind::ReturnInst(inst), IrTy::Void);

        let nxt_bb = self.ctx.build_bb_after_cur();
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
                let id = self.ctx.build_inst_end_of_cur(InstKind::Binary(inst), expr.ty.into());
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
                    let id = self.ctx.build_inst_end_of_cur(InstKind::ZExt(zext_inst), IrTy::int());
                    val = Operand::from(id);
                }
                let inst = BinaryInst {
                    op: BinaryInstOp::Ne,
                    left: val,
                    right: Operand::from(0),
                };
                let id = self.ctx.build_inst_end_of_cur(InstKind::Binary(inst), IrTy::bool());
                Ok(id.into())
            }
        }
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult {
        let left = self.visit_expr(&expr.lhs)?;
        let right = self.visit_expr(&expr.rhs)?;
        let op = expr.op.to_binary_inst_kind();

        let inst = BinaryInst { op, left, right };
        let id = self.ctx.build_inst_end_of_cur(InstKind::Binary(inst), expr.ty.into());
        Ok(id.into())
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult {
        let func = self.ctx.scope_builder.find_name_rec(&expr.func.name)
            .ok_or(Error::UnknownName(expr.func.name.clone()))?
            .as_func()
            .ok_or(Error::ExpectedFunction(expr.func.name.clone()))?
            .clone();
        let args = expr.args.iter()
            .map(|x| self.visit_expr(&x))
            .collect::<Result<Vec<_>, _>>()?;

        let ret_ty = self.ctx.get_func_ret_ty();
        let inst = CallInst { func, args };
        let id = self.ctx.build_inst_end_of_cur(InstKind::Call(inst), ret_ty);
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
