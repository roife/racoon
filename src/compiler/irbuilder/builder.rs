use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use crate::compiler::ir::instructions::{BinaryInst, CallInst};
use crate::compiler::ir::values::BasicBlock;
use crate::compiler::ptr::{MutRc, MutWeak};

use super::{
    context::ScopeBuilder,
    err::Error,
    super::ir::values::{self as ir, Ty, Use, Value},
    super::span::Span,
    super::syntax::{ast::*, visitor::AstVisitor},
};

struct Context {
    scope_builder: ScopeBuilder,
    cur_func: MutWeak<ir::Func>,
    cur_bb: MutWeak<BasicBlock>,
}

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
    type ExprResult = Result<(MutWeak<Value>, Ty), Error>;
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

    fn visit_func_param(&mut self, _param: &FuncParam) -> Self::StmtResult {
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
        let val = if let Some(val) = &stmt.val {
            Some(self.visit_expr(&val)?)
        } else {
            None
        };
        //
        // let cur_func = self.ctx.cur_func.clone();
        // let call_inst = CallInst::from(cur_func, val.0);
        // let cur_bb = self.ctx.current_bb.upgrade().unwrap();
        // let nxt_bb = MutRc::new(BasicBlock::new());
        // cur_bb.take().set_nxt_blk(nxt_bb);
        // // self.builder.func.bb_get_mut(curr_bb).branch = Branch::Return(val.map(|x| x.0));
        // //
        // // self.builder.mark_filled(self.builder.current_bb_id());
        // //
        // // let next_bb = self.builder.new_bb();
        // // self.builder.set_current_bb(next_bb);
        // // self.builder.mark_sealed(next_bb);
        // // self.builder.func.bb_set_after(curr_bb, next_bb);
        // //
        // // Ok(())
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
        let (val, ty) = self.visit_expr(&expr.expr)?;
        todo!()
    }

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::ExprResult {
        let (lhs, lhs_ty) = self.visit_expr(&expr.lhs)?;
        let (rhs, rhs_ty) = self.visit_expr(&expr.rhs)?;
        assert_type_eq(&lhs_ty, &rhs_ty)?;
        match (lhs_ty, rhs_ty) {
            (_, Ty::Void) | (Ty::Void, _) => todo!(),
            _ => todo!()
        }
        let op = expr.op.to_binary_inst_kind();
        let binary_inst = BinaryInst::from(op, lhs, rhs);
        todo!()
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::ExprResult {
        let callee = self.ctx.scope_builder.find_name_rec(&expr.func.name)?.upgrade().unwrap();
        let calle_ty = &RefCell::borrow(callee.borrow()).ty;
        let callee_ty = match calle_ty {
            Ty::Func(ty) => ty,
            _ => return todo!()
        };

        let mut arg_vals = vec![];
        let mut arg_tys = vec![];
        for arg in &expr.args {
            let (val, ty) = self.visit_expr(&arg)?;
            arg_vals.push(val);
            arg_tys.push(ty);
        }

        if arg_tys.len() != callee_ty.params.len() {
            return Err(Error::WrongParamLength {
                expected: callee_ty.params.len(),
                found: arg_tys.len(),
            });
        }
        for (ty, expected) in arg_tys.iter().zip(callee_ty.params.iter()) {
            assert_type_eq(ty, expected)?;
        }

        let call_inst = CallInst::from(MutRc::from(&callee).weak(), arg_vals);
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
