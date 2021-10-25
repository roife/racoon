use std::iter::Peekable;

use crate::compiler::span::Span;

use super::{
    ast::*,
    err::{ParseError, ParseErrorKind},
    lexer::Lexer,
    token::TokenType,
};

macro_rules! expect_token {
    ($self:expr, $pat:pat) => {
        $self.next_if(|token| matches!(token.token_type, $pat))
        .map_or(
            Err(ParseError {
                parse_error_kind: ParseErrorKind::ExpectedPattern(stringify!($pat).to_owned()),
                span: $self.peek().map_or(Span::MAX, |x| x.span),
            }),
            |token| Ok(token)
        )
    };
}

macro_rules! next_if_match {
    ($self:expr, $pat:pat) => {
        $self.next_if(|token| matches!(token.token_type, $pat)).is_some()
    };
}

macro_rules! is_next {
    ($self:expr, $pat:pat) => {
        $self.peek().map_or(false, |token| matches!(token.token_type, $pat))
    };
}

macro_rules! parse_separate_match {
    ($self:expr, $sep_pat:pat, $parse:expr) => {{
        let first = (|| $parse)()?;
        let mut v = vec![first];
        while next_if_match!($self, $sep_pat) {
            let val = (|| $parse)()?;
            v.push(val);
        }
        v
    }};
}

pub struct Parser<T>
    where T: Iterator<Item=char>,
{
    iter: Peekable<Lexer<T>>,
}

impl<T> Parser<T>
    where T: Iterator<Item=char>,
{
    pub fn new(lexer: Lexer<T>) -> Parser<T> {
        Parser {
            iter: lexer.into_iter().peekable()
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program_items = vec![];

        while self.iter.peek().is_some() {
            let is_const = next_if_match!(self.iter, TokenType::ConstKw);
            let ty = self.parse_ty()?;
            let lvalue = self.parse_lvalue()?;

            if is_next!(self.iter, TokenType::LParen) {
                let func = self.parse_func(ty, lvalue.lval_name)?;
                program_items.push(ProgramItem::Func(func));
            } else {
                let decl = self.parse_decl(is_const, ty, lvalue)?;
                program_items.push(ProgramItem::Decl(decl));
            }
        }

        Ok(Program { program_items })
    }

    fn parse_decl(&mut self, is_const: bool, ty: TypeDef, lvalue: LVal) -> Result<Decl, ParseError> {
        let mut sub_decls = vec![self.parse_sub_decl(is_const, lvalue)?];
        if next_if_match!(self.iter, TokenType::Comma) {
            let mut more_sub_decl = parse_separate_match!(self.iter, TokenType::Comma, {
                let lval = self.parse_lvalue()?;
                self.parse_sub_decl(is_const, lval)
            });
            sub_decls.append(&mut more_sub_decl);
        }

        let start = ty.span.start;
        let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;

        Ok(Decl {
            is_const,
            ty,
            sub_decls,
            span: Span { start, end },
        })
    }

    fn parse_sub_decl(&mut self, is_const: bool, lvalue: LVal) -> Result<SubDecl, ParseError> {
        let mut span = lvalue.span;
        let init_val = if is_const || is_next!(self.iter, TokenType::Assign) {
            expect_token!(self.iter, TokenType::Assign)?;
            let init_val = self.parse_init_val()?;
            span.end = init_val.span().end;
            Some(init_val)
        } else {
            None
        };

        Ok(SubDecl {
            name: lvalue.lval_name,
            subs: lvalue.subs,
            init_val,
            span,
            ty: AstTy::Unknown,
        })
    }

    fn parse_init_val(&mut self) -> Result<InitVal, ParseError> {
        let init_val = if next_if_match!(self.iter, TokenType::LBrace) {
            let array_vals = parse_separate_match!(self.iter, TokenType::Comma, {
                Ok(self.parse_init_val()?)
            });
            expect_token!(self.iter, TokenType::RBrace)?;
            InitVal::ArrayVal(array_vals)
        } else {
            let expr_val = self.parse_expr()?;
            InitVal::Expr(expr_val)
        };

        Ok(init_val)
    }

    fn parse_func(&mut self, ret_ty: TypeDef, name: Ident) -> Result<AstFunc, ParseError> {
        expect_token!(self.iter, TokenType::LParen)?;
        let params = if is_next!(self.iter, TokenType::RParen) {
            vec![]
        } else {
            parse_separate_match!(self.iter, TokenType::Comma, self.parse_func_param())
        };
        expect_token!(self.iter, TokenType::RParen)?;

        let body = self.parse_block_stmt()?;

        let (start, end) = (ret_ty.span.start, body.span.end);
        Ok(AstFunc {
            func_name: name,
            params,
            ret_ty,
            body,
            span: Span { start, end },
        })
    }

    fn parse_func_param(&mut self) -> Result<FuncParam, ParseError> {
        let ty = self.parse_ty()?;
        let name = self.parse_ident()?;
        let param_start = name.span.start;
        let mut param_end = name.span.end;
        let subs = if next_if_match!(self.iter, TokenType::LBracket) {
            expect_token!(self.iter, TokenType::RBracket)?;
            let subs = self.parse_dim()?;
            param_end = subs.span.end;
            Some(subs)
        } else {
            None
        };
        Ok(FuncParam {
            param_name: name,
            subs: subs,
            ty,
            span: Span {
                start: param_start,
                end: param_end,
            },
        })
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::LBrace)?.span.start;
        let mut block_items = vec![];
        while !is_next!(self.iter, TokenType::RBrace) {
            let block_item = if is_next!(self.iter, TokenType::IntTy | TokenType::VoidTy | TokenType::ConstKw) {
                let decl = self.parse_decl_stmt()?;
                BlockItem::Decl(decl)
            } else {
                let stmt = self.parse_stmt()?;
                BlockItem::Stmt(stmt)
            };
            block_items.push(block_item);
        }
        let end = expect_token!(self.iter, TokenType::RBrace)?.span.end;

        Ok(BlockStmt {
            block_items,
            span: Span { start, end },
        })
    }

    fn parse_decl_stmt(&mut self) -> Result<Decl, ParseError> {
        let is_const = next_if_match!(self.iter, TokenType::ConstKw);
        let ty = self.parse_ty()?;
        let sub_decls = parse_separate_match!(self.iter, TokenType::Comma, {
            let lval = self.parse_lvalue()?;
            self.parse_sub_decl(is_const, lval)
        });

        let start = ty.span.start;
        let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;

        Ok(Decl {
            is_const,
            ty,
            sub_decls,
            span: Span { start, end },
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let stmt = if is_next!(self.iter, TokenType::LBrace) {
            Stmt::Block(self.parse_block_stmt()?)
        } else if is_next!(self.iter, TokenType::IfKw) {
            Stmt::If(self.parse_if_stmt()?)
        } else if is_next!(self.iter, TokenType::WhileKw) {
            Stmt::While(self.parse_while_stmt()?)
        } else if is_next!(self.iter, TokenType::BreakKw) {
            Stmt::Break(self.parse_break_stmt()?)
        } else if is_next!(self.iter, TokenType::ContinueKw) {
            Stmt::Continue(self.parse_continue_stmt()?)
        } else if is_next!(self.iter, TokenType::ReturnKw) {
            Stmt::Return(self.parse_return_stmt()?)
        } else if is_next!(self.iter, TokenType::Semicolon) {
            let span = expect_token!(self.iter, TokenType::Semicolon)?.span;
            Stmt::Empty(span)
        } else {
            Stmt::Expr(self.parse_expr_stmt()?)
        };
        Ok(stmt)
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::IfKw)?.span.start;

        expect_token!(self.iter, TokenType::LParen)?;
        let cond = Box::new(self.parse_expr()?);
        expect_token!(self.iter, TokenType::RParen)?;

        let then_block = Box::new(self.parse_stmt()?);
        let mut end = then_block.span().end;

        let else_block = if next_if_match!(self.iter, TokenType::ElseKw) {
            let block_stmt = Box::new(self.parse_stmt()?);
            end = block_stmt.span().end;
            Some(block_stmt)
        } else {
            None
        };

        Ok(IfStmt {
            cond,
            then_block,
            else_block,
            span: Span { start, end },
        })
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::WhileKw)?.span.start;

        expect_token!(self.iter, TokenType::LParen)?;
        let cond = Box::new(self.parse_expr()?);
        expect_token!(self.iter, TokenType::RParen)?;

        let body = Box::new(self.parse_stmt()?);
        let end = body.span().end;

        Ok(WhileStmt {
            cond,
            body,
            span: Span { start, end },
        })
    }

    fn parse_break_stmt(&mut self) -> Result<Span, ParseError> {
        let start = expect_token!(self.iter, TokenType::BreakKw)?.span.start;
        let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;
        Ok(Span { start, end })
    }

    fn parse_continue_stmt(&mut self) -> Result<Span, ParseError> {
        let start = expect_token!(self.iter, TokenType::ContinueKw)?.span.start;
        let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;
        Ok(Span { start, end })
    }

    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::ReturnKw)?.span.start;
        let val = if is_next!(self.iter, TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expr()?))
        };
        let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;

        Ok(ReturnStmt {
            val,
            span: Span { start, end },
        })
    }

    fn parse_expr_stmt(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr()?;
        expect_token!(self.iter, TokenType::Semicolon)?;
        return Ok(expr);
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_unary_expr()?;
        self.parse_expr_opg(lhs, 0)
    }

    fn parse_expr_opg(&mut self, lhs: Expr, prec: u32) -> Result<Expr, ParseError> {
        let mut lhs = lhs;
        while let Some(op_token) = self.iter.next_if(|token| {
            let op = &token.token_type;
            op.is_binary_op() && op.prec() >= prec
        }) {
            // OPG
            let op = op_token.token_type;
            let mut rhs = self.parse_unary_expr()?;

            while self.iter.peek().map_or(false, |next_token| {
                let next_op = &next_token.token_type;
                next_op.is_binary_op() && if next_op.is_left_assoc() {
                    next_op.prec() > op.prec()
                } else {
                    next_op.prec() == op.prec()
                }
            }) {
                let next_op_prec = self.iter.peek().unwrap().token_type.prec();
                rhs = self.parse_expr_opg(rhs, next_op_prec)?;
            }

            // combine
            let span = Span {
                start: lhs.span().start,
                end: rhs.span().end,
            };

            lhs = match op {
                TokenType::Assign => {
                    Expr::Assign(AssignExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        allow_assign_const: false,
                        span,
                        ty: AstTy::Unknown,
                    })
                }
                _ => Expr::Binary(BinaryExpr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: op.to_binary_op().unwrap(),
                    span,
                    ty: AstTy::Unknown,
                })
            };
        }
        Ok(lhs)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        let mut pre_op_tokens = vec![];
        while self.iter.peek().map_or(false, |x| x.token_type.is_unary_op()) {
            pre_op_tokens.push(self.iter.next().unwrap())
        }

        let mut expr_item = self.parse_expr_item()?;
        let end = expr_item.span().end;
        for prec_op in pre_op_tokens.drain(..).rev() {
            let op = prec_op.token_type.to_unary_op().unwrap();
            expr_item = Expr::Unary(UnaryExpr {
                op,
                expr: Box::new(expr_item),
                span: Span { start: prec_op.span.start, end },
                ty: AstTy::Unknown,
            });
        }
        Ok(expr_item)
    }

    fn parse_expr_item(&mut self) -> Result<Expr, ParseError> {
        if is_next!(self.iter, TokenType::Ident(_)) {
            let lvalue = self.parse_lvalue()?;

            if is_next!(self.iter, TokenType::LParen) {
                let call = self.parse_func_call(lvalue.lval_name)?;
                Ok(Expr::Call(call))
            } else {
                Ok(Expr::LVal(lvalue))
            }
        } else if is_next!(self.iter, TokenType::IntLiteral(_)) {
            let int_literal = expect_token!(self.iter, TokenType::IntLiteral(_))?;
            Ok(Expr::Literal(LiteralExpr {
                kind: LiteralKind::Integer(*int_literal.token_type.as_int_literal().unwrap()),
                span: int_literal.span,
                ty: AstTy::Unknown,
            }))
        } else if is_next!(self.iter, TokenType::LParen) {
            expect_token!(self.iter, TokenType::LParen)?;
            let expr = self.parse_expr()?;
            expect_token!(self.iter, TokenType::RParen)?;
            Ok(expr)
        } else {
            Err(ParseError {
                parse_error_kind: ParseErrorKind::ExpectedPattern(
                    String::from("Literal or Identifier or Function call or Parenthesis")
                ),
                span: self.iter.peek().map_or(Span::MAX, |x| x.span),
            })
        }
    }

    fn parse_func_call(&mut self, func: Ident) -> Result<CallExpr, ParseError> {
        let start = func.span.start;
        expect_token!(self.iter, TokenType::LParen)?;
        let params = parse_separate_match!(self.iter,TokenType::Comma,self.parse_expr());
        let end = expect_token!(self.iter, TokenType::RParen)?.span.end;

        Ok(CallExpr {
            func,
            args: params,
            span: Span { start, end },
            ty: AstTy::Unknown,
        })
    }

    fn parse_lvalue(&mut self) -> Result<LVal, ParseError> {
        let name = self.parse_ident()?;
        let mut span = name.span;
        let subs = if is_next!(self.iter, TokenType::LBracket) {
            let subs = self.parse_dim()?;
            span.end = subs.span.end;
            Some(subs)
        } else {
            None
        };

        Ok(LVal { lval_name: name, subs: subs, span, ty: AstTy::Unknown })
    }

    fn parse_dim(&mut self) -> Result<Subs, ParseError> {
        let mut span = expect_token!(self.iter, TokenType::LBracket).unwrap().span;
        let subs = parse_separate_match!(self.iter, TokenType::LBracket, {
            let dim = self.parse_expr()?;
            span.end = expect_token!(self.iter, TokenType::RBracket)?.span.end;
            Ok(dim)
        });
        Ok(Subs { subs: subs, span })
    }

    fn parse_ty(&mut self) -> Result<TypeDef, ParseError> {
        let ty_token = expect_token!(self.iter, TokenType::IntTy | TokenType::VoidTy)?;
        Ok(TypeDef {
            ty_ident: ty_token.token_type.to_ty_ident().unwrap(),
            span: ty_token.span,
        })
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let token = expect_token!(self.iter, TokenType::Ident(_))?;
        Ok(Ident {
            span: token.span,
            name: token.token_type.as_ident().unwrap().to_string(),
        })
    }
}
