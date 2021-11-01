use itertools::Itertools;

use crate::compiler::span::Span;
use crate::compiler::syntax::ast::*;
use crate::compiler::syntax::visitor::AstVisitorMut;

use super::{
    context::ScopeBuilder,
    err::SemanticError::{self, *},
};

macro_rules! expect_type {
    ($self:expr, $pat:pat) => {{
        if matches!($self, $pat) {
            Err(TypeMismatch {
                expected: String::from(stringify!($pat)),
                found: $self
            })
        } else {
            Ok(())
        }
    }};
}

pub struct TypeChecker {
    pub scopes: ScopeBuilder<(AstTy, Option<LiteralExpr>)>,
    pub cur_func_ret_ty: AstTy,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            scopes: ScopeBuilder::new(),
            cur_func_ret_ty: AstTy::Unknown,
        }
    }
}

impl TypeChecker {
    fn fix_array_literal(literal: &mut LiteralExpr, expected_ty: &AstTy) -> Result<(), SemanticError> {
        match (&mut literal.kind, expected_ty) {
            (LiteralKind::Integer(_), AstTy::Int) => Ok(()),
            (LiteralKind::Array(literal_siz, literal_vals),
                AstTy::Array { siz: ty_siz, elem_ty })
            => {
                if *literal_siz > *ty_siz {
                    return Err(SemanticError::TooMuchElement);
                }

                literal_vals.iter_mut()
                    .try_for_each(|x| Self::fix_array_literal(x, elem_ty))?;

                // fix array literal
                *literal_siz = *ty_siz;
                literal.ty = expected_ty.clone();
                Ok(())
            }
            _ => assert_type_eq(&expected_ty, &AstTy::Unknown)
        }
    }

    fn build_ast_ty(&mut self, base_ty: &AstTy, subs: &mut Option<Subs>) -> Result<AstTy, SemanticError> {
        if subs.is_some() {
            for sub in subs.as_mut().unwrap().subs.iter_mut() {
                let literal = self.visit_expr(sub)?
                    .ok_or(SemanticError::NotConstant)?;

                if literal.get_int().unwrap() <= 0 {
                    return Err(SemanticError::IllegalArrayDim);
                }
                *sub = Expr::Literal(literal);
            }
        }
        let mut ty = base_ty.clone();
        if let Some(subs) = &subs {
            for sub in subs.subs.iter().rev() {
                ty = AstTy::Array {
                    siz: sub.as_literal().unwrap().get_int().unwrap() as usize,
                    elem_ty: Box::new(ty.clone()),
                };
            }
        }
        Ok(ty)
    }
}

impl AstVisitorMut for TypeChecker {
    type ProgramResult = Result<(), SemanticError>;
    type ConstInitValResult = Result<LiteralExpr, SemanticError>;
    type FuncResult = Result<(), SemanticError>;
    type StmtResult = Result<(), SemanticError>;
    type ExprResult = Result<Option<LiteralExpr>, SemanticError>;
    type LExprResult = Result<Option<LiteralExpr>, SemanticError>;
    type TyResult = Result<AstTy, SemanticError>;

    fn visit_program(&mut self, program: &mut Program) -> Self::ProgramResult {
        self.scopes.push_scope();
        program.program_items.iter_mut().try_for_each(|item| {
            match item {
                ProgramItem::Decl(x) => self.visit_global_decl(x),
                ProgramItem::Func(x) => self.visit_func(x),
            }
        })?;
        self.scopes.pop_scope();
        Ok(())
    }

    fn visit_const_init_val(&mut self, init_val: &mut InitVal) -> Self::ConstInitValResult {
        match &mut init_val.kind {
            InitValKind::Expr(x) => {
                init_val.ty = x.ty();
                match self.visit_expr(x)? {
                    Some(x) => Ok(x),
                    None => Err(SemanticError::NotConstant),
                }
            }
            InitValKind::ArrayVal(vals) => {
                let literals: Vec<_> = vals.iter_mut()
                    .map(|x| self.visit_const_init_val(x))
                    .try_collect()?;
                Ok(LiteralExpr {
                    kind: LiteralKind::Array(literals.len(), literals),
                    span: init_val.span,
                    ty: AstTy::Unknown,
                })
            }
            _ => unreachable!()
        }
    }

    fn visit_global_decl(&mut self, decl: &mut Decl) -> Self::StmtResult {
        let ty = self.visit_ty(&mut decl.ty_ident)?;

        for sub_decl in decl.sub_decls.iter_mut() {
            let ty = self.build_ast_ty(&ty, &mut sub_decl.subs)?;

            let init_val = if let Some(init_val) = &mut sub_decl.init_val {
                let mut literal = self.visit_const_init_val(init_val)?;
                match ty {
                    AstTy::Int => assert_type_eq(&ty, &literal.ty)?,
                    AstTy::Array { .. } => Self::fix_array_literal(&mut literal, &ty)?,
                    _ => unreachable!()
                };
                init_val.kind = InitValKind::Const(literal.clone());
                Some(literal)
            } else {
                None
            };

            sub_decl.ty = ty.clone();
            self.scopes.insert(&sub_decl.ident.name, (ty.clone(), init_val));
        }
        Ok(())
    }

    fn visit_func(&mut self, ast_func: &mut AstFunc) -> Self::FuncResult {
        let ret_ty = Box::new(self.visit_ty(&mut ast_func.ret_ty_ident)?);

        ast_func.params.iter_mut().try_for_each(|param| self.visit_func_param(param))?;
        let param_tys = ast_func.params.iter()
            .map(|x| Box::new(x.ty.clone()))
            .collect();

        let func_ty = AstTy::Func { ret_ty, param_tys };
        self.scopes.insert(&ast_func.ident.name, (func_ty.clone(), None))
            .ok_or(SemanticError::DuplicateName(ast_func.ident.name.clone()))?;

        self.scopes.push_scope();
        for param in ast_func.params.iter() {
            self.scopes.insert(&param.ident.name, (param.ty.clone(), None))
                .ok_or(SemanticError::DuplicateName(param.ident.name.clone()))?;
        }

        self.visit_block_stmt(&mut ast_func.body)?;
        self.scopes.pop_scope();
        Ok(())
    }

    fn visit_func_param(&mut self, param: &mut FuncParam) -> Self::StmtResult {
        let base_ty = self.visit_ty(&mut param.ty_ident)?;
        let mut ty = self.build_ast_ty(&base_ty, &mut param.subs)?;
        if let AstTy::Array { .. } = &ty {
            ty = AstTy::Array { siz: 0, elem_ty: Box::new(ty) };
        }
        param.ty = ty;
        Ok(())
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
        let ret_ty = match &stmt.val {
            None => AstTy::Void,
            Some(val) => val.ty()
        };

        if let Some(expr) = &mut stmt.val {
            self.visit_expr(expr)?;
        }

        let expected = &self.cur_func_ret_ty;

        assert_type_eq(&ret_ty, expected)
    }

    fn visit_empty_stmt(&mut self, _span: Span) -> Self::StmtResult {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult {
        match expr {
            Expr::LVal(_) => self.visit_lexpr(expr),
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
        expr.ty = match &mut expr.kind {
            LiteralKind::Integer(_) => AstTy::Int,
            _ => unreachable!()
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
            (AstTy::Int, AstTy::Int) => matches!(op, Add | Sub | Mul | Div | Mod | Lt | Le | Gt | Ge | Eq | Ne),
            (AstTy::Bool, AstTy::Bool) => matches!(op, And | Or),
            _ => false,
        };

        if !legal {
            return Err(SemanticError::TypeMismatch {
                expected: String::from(stringify!(lty)),
                found: expr.rhs.ty().into(),
            })
        }

        let new_ty = match op {
            Add | Sub | Mul | Div | Mod => AstTy::Int,
            Lt | Le | Gt | Ge | Eq | Ne | And | Or => AstTy::Bool,
        };

        expr.ty = new_ty.clone();

        let val = if let (
            Some(LiteralExpr { kind: LiteralKind::Integer(lval), span: lspan, .. }),
            Some(LiteralExpr { kind: LiteralKind::Integer(rval), span: rspan, .. })
        ) = (lval, rval) {
            let new_val = match op {
                Add => lval + rval,
                Sub => lval - rval,
                Mul => lval * rval,
                Div => lval / rval,
                Mod => lval % rval,
                Lt => (lval < rval) as i32,
                Le => (lval <= rval) as i32,
                Gt => (lval > rval) as i32,
                Ge => (lval >= rval) as i32,
                Eq => (lval == rval) as i32,
                Ne => (lval != rval) as i32,
                And => (lval != 0 && rval != 0) as i32,
                Or => (lval != 0 || rval != 0) as i32,
            };
            Some(LiteralExpr {
                kind: LiteralKind::Integer(new_val),
                span: Span { start: lspan.start, end: rspan.end },
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
            .0.as_func()
            .ok_or(SemanticError::ExpectedFunction(expr.func.name.clone()))?;

        expr.args.iter().zip(param_tys)
            .try_for_each(|(x, y)| assert_type_eq(&x.ty(), y))?;

        expr.ty = ret_ty.as_ref().clone();
        Ok(None)
    }

    fn visit_ty(&mut self, ty_def: &mut TypeIdent) -> Self::TyResult {
        let ty = match &ty_def.ty_ident {
            TyIdent::Primitive(prim_ty) => match prim_ty {
                PrimitiveTy::Integer => AstTy::Int
            }
            TyIdent::Void => AstTy::Void
        };
        Ok(ty)
    }
}

fn assert_type_eq(expected: &AstTy, found: &AstTy) -> Result<(), SemanticError> {
    if expected != found {
        return Err(TypeMismatch {
            expected: String::from(stringify!(lhs)),
            found: found.clone(),
        });
    }
    Ok(())
}