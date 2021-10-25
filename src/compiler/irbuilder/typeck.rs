use crate::compiler::irbuilder::context::ScopeBuilder;
use crate::compiler::irbuilder::err::SemanticError;
use crate::compiler::span::Span;
use crate::compiler::syntax::ast::*;
use crate::compiler::syntax::ast::AstTy;
use crate::compiler::syntax::visitor::AstVisitorMut;

use super::err::SemanticError::*;

macro_rules! expect_type {
    ($self:expr, $pat:pat) => {{
        let ty = $self.clone();
        if matches!(ty, $pat) {
            Err(TypeMismatch {
                expected: String::from(stringify!($pat)),
                found: $self
            })
        } else {
            Ok(ty)
        }
    }};
}

pub struct TypeChecker {
    pub scopes: ScopeBuilder<AstTy>,
    pub cur_func_ret_ty: AstTy,
}

impl AstVisitorMut for TypeChecker {
    type ProgramResult = Result<(), SemanticError>;
    type ConstInitValResult = Result<Option<LiteralExpr>, SemanticError>;
    type FuncResult = Result<(), SemanticError>;
    type StmtResult = Result<(), SemanticError>;
    type ExprResult = Result<Option<LiteralExpr>, SemanticError>;
    type LExprResult = Result<Option<LiteralExpr>, SemanticError>;
    type TyResult = Result<AstTy, SemanticError>;

    fn visit_program(&mut self, program: &mut Program) -> Self::ProgramResult {
        program.program_items.iter_mut().try_for_each(|item| {
            match item {
                ProgramItem::Decl(x) => self.visit_global_decl(x),
                ProgramItem::Func(x) => self.visit_func(x),
            }
        })
    }

    fn visit_global_decl(&mut self, decl: &mut Decl) -> Self::StmtResult {
        todo!()
    }

    fn visit_func(&mut self, func: &mut AstFunc) -> Self::FuncResult {
        todo!()
    }

    fn visit_func_param(&mut self, _param: &mut FuncParam) -> Self::StmtResult {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmt: &mut BlockStmt) -> Self::StmtResult {
        self.scopes.push_scope();
        stmt.block_items.iter_mut().try_for_each(|sub_stmt| match sub_stmt {
            BlockItem::Stmt(x) => self.visit_stmt(x),
            BlockItem::Decl(x) => self.visit_decl_stmt(x),
        })?;
        self.scopes.pop_scope();
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult {
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

    fn visit_decl_stmt(&mut self, decl: &mut Decl) -> Self::StmtResult {
        todo!()
    }

    fn visit_expr_stmt(&mut self, stmt: &mut Expr) -> Self::StmtResult {
        self.visit_expr(stmt)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &mut IfStmt) -> Self::StmtResult {
        self.visit_expr(&mut stmt.cond)?;
        expect_type!(stmt.cond.ty(), AstTy::Int)?;
        self.visit_stmt(&mut stmt.then_block)?;
        if let Some(else_blk) = &mut stmt.else_block {
            self.visit_stmt(else_blk)?;
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &mut WhileStmt) -> Self::StmtResult {
        self.visit_expr(&mut stmt.cond)?;
        expect_type!(stmt.cond.ty(), AstTy::Int)?;
        self.visit_stmt(&mut stmt.body)?;
        Ok(())
    }

    fn visit_break_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_continue_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &mut ReturnStmt) -> Self::StmtResult {
        // let ret_ty = match &stmt.val {
        //     None => AstTy::Void,
        //     Some(val) => val.ty().clone()
        // };
        // let expected = self.cur_func_ret_ty.clone();
        // expect_type!(ret_ty, expected);
        todo!()
    }

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult {
        match expr {
            Expr::LVal(x) => self.visit_lexpr(expr),
            Expr::Assign(x) => self.visit_assign_expr(x),
            Expr::Literal(x) => self.visit_literal_expr(x),
            Expr::Unary(x) => self.visit_unary_expr(x),
            Expr::Binary(x) => self.visit_binary_expr(x),
            Expr::Call(x) => self.visit_call_expr(x)
        }
    }

    fn visit_lexpr(&mut self, _expr: &mut Expr) -> Self::LExprResult {
        todo!()
    }

    fn visit_assign_expr(&mut self, expr: &mut AssignExpr) -> Self::ExprResult {
        todo!()
    }

    fn visit_literal_expr(&mut self, expr: &mut LiteralExpr) -> Self::ExprResult {
        expr.ty = match expr.kind {
            LiteralKind::Integer(_) => AstTy::Int
        };
        Ok(Some(expr.clone()))
    }

    fn visit_unary_expr(&mut self, expr: &mut UnaryExpr) -> Self::ExprResult {
        let val = self.visit_expr(&mut expr.sub_expr)?;
        let sub_expr_ty = expr.sub_expr.ty();
        let res = match expr.op {
            UnaryOp::Neg => {
                expect_type!(sub_expr_ty, AstTy::Int)?;
                expr.ty = AstTy::Int;
                val.and_then(|x| x.get_int())
                    .map(|x| LiteralExpr {
                        kind: LiteralKind::Integer(-x),
                        span: expr.span,
                        ty: AstTy::Int,
                    })
            }
            UnaryOp::Pos => {
                expect_type!(sub_expr_ty, AstTy::Int)?;
                expr.ty = AstTy::Int;
                val
            }
            UnaryOp::Not => {
                expect_type!(sub_expr_ty, AstTy::Int | AstTy::Bool)?;
                expr.ty = expr.sub_expr.ty();
                val.and_then(|x| x.get_int())
                    .map(|x| LiteralExpr {
                        kind: LiteralKind::Integer((x != 0) as i32),
                        span: expr.span,
                        ty: expr.ty.clone(),
                    })
            }
        };
        Ok(res)
    }

    fn visit_binary_expr(&mut self, expr: &mut BinaryExpr) -> Self::ExprResult {
        use BinaryOp::*;
        let lval = self.visit_expr(&mut expr.lhs)?;
        let rval = self.visit_expr(&mut expr.rhs)?;
        let op = expr.op;

        let legal = match (expr.lhs.ty(), expr.rhs.ty()) {
            (AstTy::Int, AstTy::Int) => {
                matches!(op, Add | Sub | Mul | Div | Mod | Lt | Le | Gt | Ge | Eq | Ne)
            }
            (AstTy::Bool, AstTy::Bool) => {
                matches!(op, And | Or)
            }
            _ => false,
        };

        if !legal {
            return Err(SemanticError::TypeMismatch {
                expected: String::from(stringify!(lty)),
                found: expr.rhs.ty().into(),
            })
        }

        let val = if let (
            Some(LiteralExpr { kind: LiteralKind::Integer(lval), span: lspan, .. }),
            Some(LiteralExpr { kind: LiteralKind::Integer(rval), span: rspan, .. })
        ) = (lval, rval) {
            let span = Span {
                start: lspan.start,
                end: rspan.end,
            };
            let (new_val, new_ty) = match op {
                Add => (lval + rval, AstTy::Int),
                Sub => (lval - rval, AstTy::Int),
                Mul => (lval * rval, AstTy::Int),
                Div => (lval / rval, AstTy::Int),
                Mod => (lval % rval, AstTy::Int),
                Lt => ((lval < rval) as i32, AstTy::Bool),
                Le => ((lval <= rval) as i32, AstTy::Bool),
                Gt => ((lval > rval) as i32, AstTy::Bool),
                Ge => ((lval >= rval) as i32, AstTy::Bool),
                Eq => ((lval == rval) as i32, AstTy::Bool),
                Ne => ((lval != rval) as i32, AstTy::Bool),
                And => ((lval != 0 && rval != 0) as i32, AstTy::Bool),
                Or => ((lval != 0 || rval != 0) as i32, AstTy::Bool),
            };
            Some(LiteralExpr {
                kind: LiteralKind::Integer(new_val),
                span,
                ty: new_ty,
            })
        } else {
            None
        };

        Ok(val)
    }

    fn visit_call_expr(&mut self, expr: &mut CallExpr) -> Self::ExprResult {
        expr.args.iter_mut()
            .try_for_each(|x| self.visit_expr(x).map(|_| ()))?;

        let (ret_ty, param_tys) = self.scopes.find_name_rec(&expr.func.name)
            .ok_or(SemanticError::UnknownName(expr.func.name.clone()))?
            .as_func()
            .ok_or(SemanticError::ExpectedFunction(expr.func.name.clone()))?;

        expr.args.iter().zip(param_tys)
            .try_for_each(|(x, y)| assert_type_eq(&x.ty(), &y))?;

        expr.ty = ret_ty.as_ref().clone();
        Ok(None)
    }

    fn visit_ty(&mut self, ty_def: &mut TypeDef) -> Self::TyResult {
        let ty = match &ty_def.ty_ident {
            TyIdent::Primitive(prim_ty) => match prim_ty {
                PrimitiveTy::Integer => AstTy::Int
            }
            TyIdent::Void => AstTy::Void
        };
        Ok(ty)
    }
}

fn assert_type_eq(lhs: &AstTy, rhs: &AstTy) -> Result<(), SemanticError> {
    if lhs != rhs {
        return Err(TypeMismatch {
            expected: String::from(stringify!(lhs)),
            found: rhs.clone(),
        });
    }
    Ok(())
}