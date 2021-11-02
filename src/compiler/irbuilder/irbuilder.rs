use itertools::Itertools;

use crate::compiler::ir::{
    arena::BBId,
    value::{
        constant::Constant,
        func::IrFunc,
        global::Global,
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
    context::{BCTarget, Context, NameId},
    err::SemanticError,
};

pub struct IrBuilder {
    pub ctx: Context,
    loop_targets: Vec<BCTarget>,
}

impl IrBuilder {
    pub fn new() -> IrBuilder {
        IrBuilder {
            ctx: Context::new(),
            loop_targets: vec![],
        }
    }

    pub fn visit(&mut self, program: &Program) -> Result<(), SemanticError> {
        self.visit_program(program)?;
        Ok(())
    }

    pub fn push_break_target(&mut self, break_target: BBId, continue_target: BBId) {
        self.loop_targets.push(BCTarget {
            break_target,
            continue_target,
        });
    }

    pub fn pop_loop_target(&mut self) {
        self.loop_targets.pop();
    }

    pub fn get_break_target(&self) -> Option<BBId> {
        Some(self.loop_targets.last()?.break_target)
    }

    pub fn get_continue_target(&self) -> Option<BBId> {
        Some(self.loop_targets.last()?.continue_target)
    }
}

impl AstVisitor for IrBuilder {
    type ProgramResult = Result<(), SemanticError>;
    type ConstInitValResult = Result<Constant, SemanticError>;
    type FuncResult = Result<(), SemanticError>;
    type StmtResult = Result<(), SemanticError>;
    type ExprResult = Result<Operand, SemanticError>;
    type LExprResult = Result<Operand, SemanticError>;
    type TyResult = Result<IrTy, SemanticError>;

    fn visit_program(&mut self, program: &Program) -> Self::ProgramResult {
        self.ctx.scope_builder.push_scope();
        program.program_items.iter()
            .try_for_each(|item| match item {
                ProgramItem::Decl(x) => self.visit_global_decl(x),
                ProgramItem::Func(x) => self.visit_func(x),
            })?;
        self.ctx.scope_builder.pop_scope();
        Ok(())
    }

    fn visit_const_init_val(&mut self, init_val: &InitVal) -> Self::ConstInitValResult {
        match init_val.kind.as_const() {
            Some(literal) => Ok(literal.clone().into()),
            None => Err(SemanticError::RequireConstant)
        }
    }

    fn visit_global_decl(&mut self, decl: &Decl) -> Self::StmtResult {
        for sub_decl in &decl.sub_decls {
            let ptr_ty = match sub_decl.ty.clone().into() {
                int @ IrTy::Int(_) => IrTy::ptr_of(int),
                arr @ IrTy::Array(_, _) => arr,
                _ => unreachable!()
            };

            let const_init_val = if let Some(init_val) = &sub_decl.init_val {
                self.visit_const_init_val(init_val)?
            } else {
                Constant::build_zero(sub_decl.ty.clone().into())
            };

            let global = self.ctx.build_global(Global::new(
                ptr_ty,
                &sub_decl.ident.name,
                const_init_val));
            self.ctx.scope_builder.insert(&sub_decl.ident.name, NameId::Global(global));
        }
        Ok(())
    }

    fn visit_func(&mut self, ast_func: &AstFunc) -> Self::FuncResult {
        let ret_ty = self.visit_ty(&ast_func.ret_ty_ident)?;
        let func = self.ctx.build_func(IrFunc::new(&ast_func.ident.name, ret_ty, false));
        self.ctx.scope_builder.insert(&ast_func.ident.name, NameId::Func(func));

        self.ctx.scope_builder.push_scope();
        self.ctx.set_cur_func(func);

        let init_bb = self.ctx.build_bb();
        self.ctx.set_cur_bb(init_bb);
        ast_func.params.iter().try_for_each(|param| self.visit_func_param(param))?;
        self.visit_block_stmt(&ast_func.body)?;

        self.ctx.scope_builder.pop_scope();
        Ok(())
    }

    fn visit_func_param(&mut self, param: &FuncParam) -> Self::StmtResult {
        let ty = IrTy::from(param.ty.clone());
        let param_id = self.ctx.build_func_param(ty.clone());

        self.ctx.scope_builder.insert(&param.ident.name, NameId::Param(param_id))
            .ok_or(SemanticError::DuplicateName(param.ident.name.clone()))?;

        let alloca_addr = self.ctx.build_inst_end_of_cur(
            InstKind::Alloca(AllocaInst { alloca_ty: ty.clone() }),
            IrTy::ptr_of(ty.clone()),
        );
        self.ctx.build_inst_end_of_cur(
            InstKind::Store(StoreInst {
                addr: Operand::Inst(alloca_addr),
                data: Operand::Param(param_id),
            }),
            IrTy::Void,
        );
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::StmtResult {
        self.ctx.scope_builder.push_scope();
        stmt.block_items.iter().try_for_each(|sub_stmt| match sub_stmt {
            BlockItem::Stmt(x) => self.visit_stmt(x),
            BlockItem::Decl(x) => self.visit_decl_stmt(x),
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

    fn visit_decl_stmt(&mut self, decl: &Decl) -> Self::StmtResult {
        for sub_decl in &decl.sub_decls {
            let ty = IrTy::from(sub_decl.ty.clone());

            let alloca_addr = self.ctx.build_inst_end_of_cur(
                InstKind::Alloca(AllocaInst { alloca_ty: ty.clone() }),
                IrTy::ptr_of(ty.clone()),
            );
            self.ctx.scope_builder.insert(&sub_decl.ident.name, NameId::Inst(alloca_addr));

            if let Some(init_val) = &sub_decl.init_val {
                let init_expr_id = self.visit_expr(init_val.kind.as_expr().unwrap())?;
                let store_inst = self.ctx.build_inst_end_of_cur(
                    InstKind::Store(StoreInst { addr: Operand::Inst(alloca_addr), data: init_expr_id }),
                    IrTy::ptr_of(IrTy::Void),
                );
                self.ctx.scope_builder.insert(&sub_decl.ident.name, NameId::Inst(store_inst));
            }
        }
        Ok(())
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
        self.visit_stmt(&stmt.then_block)?;
        let then_bb_end = self.ctx.get_cur_bb_id();

        // else bb
        let (else_bb, else_bb_end) = &stmt.else_block.as_ref()
            .map_or(Ok((None, None)), |else_blk| {
                let else_bb = self.ctx.build_bb_after_cur();
                self.ctx.set_cur_bb(else_bb);
                self.visit_stmt(&else_blk)?;
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
        self.push_break_target(nxt_bb, cond_bb);
        self.visit_stmt(&stmt.body)?;
        self.pop_loop_target();
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
        let break_target = self.get_break_target()
            .ok_or(SemanticError::BreakOutsideLoop)?;
        self.ctx.build_inst_end_of_cur(
            InstKind::Branch(BranchInst::Jump { nxt_bb: break_target }),
            IrTy::Void);
        let nxt_bb = self.ctx.build_bb_after_cur();
        self.ctx.set_cur_bb(nxt_bb);
        Ok(())
    }

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult {
        let continue_target = self.get_continue_target()
            .ok_or(SemanticError::ContinueOutsideLoop)?;
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
        match expr {
            Expr::LVal(_) => self.visit_lexpr(expr),
            Expr::Assign(x) => self.visit_assign_expr(x),
            Expr::Literal(x) => self.visit_literal_expr(x),
            Expr::Unary(x) => self.visit_unary_expr(x),
            Expr::Binary(x) => self.visit_binary_expr(x),
            Expr::Call(x) => self.visit_call_expr(x),
        }
    }

    fn visit_lexpr(&mut self, expr: &Expr) -> Self::LExprResult {
        // let val= expr.as_l_val().unwrap();
        // let base_addr = self.ctx.scope_builder.find_name_rec(&val.ident.name)
        //    .ok_or(SemanticError::UnknownName(val.ident.name.clone()))?;
        todo!()
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::ExprResult {
        // let lv_addr = self.visit_lexpr(&expr.lhs)?;
        // let rv = self.visit_expr(&expr.rhs)?;
        // let inst = StoreInst { addr: lv_addr, data: rv };
        // self.ctx.build_inst_end_of_cur(InstKind::Store(inst), IrTy::Void);
        todo!()
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::ExprResult {
        let constant = match expr.kind {
            LiteralKind::Integer(i) => Operand::Constant(Constant::Int(i)),
            _ => unreachable!()
        };
        Ok(constant)
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::ExprResult {
        let mut val = self.visit_expr(&expr.sub_expr)?;
        match expr.op {
            UnaryOp::Neg => {
                let inst = BinaryInst {
                    op: BinaryInstOp::Sub,
                    left: Operand::from(0),
                    right: val,
                };
                let id = self.ctx.build_inst_end_of_cur(InstKind::Binary(inst), expr.ty.clone().into());
                Ok(id.into())
            }
            UnaryOp::Pos => Ok(val),
            UnaryOp::Not => {
                let sub_expr_ty = expr.sub_expr.ty();
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
        let id = self.ctx.build_inst_end_of_cur(InstKind::Binary(inst), expr.ty.clone().into());
        Ok(id.into())
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult {
        let func = self.ctx.scope_builder.find_name_rec(&expr.func.name).unwrap()
            .as_func().unwrap()
            .clone();
        let args = expr.args.iter()
            .map(|x| self.visit_expr(&x))
            .try_collect()?;

        let ret_ty = self.ctx.get_func_ty(func).ret_ty;
        let inst = CallInst { func, args };
        let id = self.ctx.build_inst_end_of_cur(InstKind::Call(inst), ret_ty);
        Ok(id.into())
    }

    fn visit_ty(&mut self, ty_def: &TypeIdent) -> Self::TyResult {
        let ty = match &ty_def.ty_ident {
            TyIdent::Primitive(prim_ty) => match prim_ty {
                PrimitiveTy::Integer => IrTy::Int(32)
            }
            TyIdent::Void => IrTy::Void
        };
        Ok(ty)
    }
}
