use std::{
    iter::Peekable,
    rc::Rc
};

use crate::compiler::span::Span;

use super::{
    ast::*,
    err::{ParseError, ParseErrorKind},
    lexer::Lexer,
    token::TokenType,
};

macro_rules! expect_token {
    ($self:expr, $($pat:pat)|+) => {
        $self.next_if(|token| matches!(token.token_type, $($pat)|+))
        .map_or(
            Err(ParseError {
                parse_error_kind: ParseErrorKind::ExpectedPattern(stringify!($($pat)|+).to_owned()),
                span: $self.peek().map_or(Span::MAX, |x| x.span),
            }),
            |token| Ok(token)
        )
    };
}

macro_rules! next_if_match {
    ($self:expr, $($pat:pat)|+) => {
        $self.next_if(|token| matches!(token.token_type, $($pat)|+)).is_some()
    };
}

macro_rules! is_next {
    ($self:expr, $($pat:pat)|+) => {
        $self.peek().map_or(false, |token| matches!(token.token_type, $($pat)|+))
    };
}

macro_rules! parse_separate_match {
    ($self:expr, $($sep_pat:pat)|+, $parse:expr) => {{
        let first = (|| $parse)()?;
        let mut v = vec![first];
        while next_if_match!($self, $($sep_pat)|+) {
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
            let l_value = self.parse_l_value()?;

            if is_next!(self.iter, TokenType::LParen) {
                let func = self.parse_func(ty, l_value.name)?;
                program_items.push(ProgramItem::Func(func));
            } else {
                let decl = self.parse_decl(is_const, ty, l_value)?;
                program_items.push(ProgramItem::Decl(decl));
            }
        }

        Ok(Program { program_items })
    }

    fn parse_decl(&mut self, is_const: bool, ty: TypeDef, l_value: LVal) -> Result<Decl, ParseError> {
        let mut sub_decls = vec![self.parse_sub_decl(is_const, l_value)?];
        if next_if_match!(self.iter, TokenType::Comma) {
            let mut more_sub_decl = parse_separate_match!(self.iter, TokenType::Comma, {
                let lval = self.parse_l_value()?;
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

    fn parse_sub_decl(&mut self, is_const: bool, l_value: LVal) -> Result<SubDecl, ParseError> {
        let mut span = l_value.span;
        let init_val = if is_const || is_next!(self.iter, TokenType::Assign) {
            expect_token!(self.iter, TokenType::Assign)?;
            let init_val = self.parse_init_val()?;
            span.end = init_val.span().end;
            Some(init_val)
        } else {
            None
        };

        Ok(SubDecl {
            name: l_value.name,
            dims: l_value.dims,
            init_val,
            span,
        })
    }

    fn parse_init_val(&mut self) -> Result<InitVal, ParseError> {
        let init_val = if next_if_match!(self.iter, TokenType::LBrace) {
            let array_vals = parse_separate_match!(self.iter, TokenType::Comma, {
                Ok(Rc::new(self.parse_init_val()?))
            });
            expect_token!(self.iter, TokenType::RBrace)?;
            InitVal::ArrayVal(array_vals)
        } else {
            let expr_val = self.parse_expr()?;
            InitVal::Expr(expr_val)
        };

        Ok(init_val)
    }

    fn parse_func(&mut self, ret_ty: TypeDef, name: Ident) -> Result<Func, ParseError> {
        expect_token!(self.iter, TokenType::LParen)?;
        let params = if is_next!(self.iter, TokenType::RParen) {
            vec![]
        } else {
            parse_separate_match!(self.iter, TokenType::Comma, self.parse_func_param())
        };
        expect_token!(self.iter, TokenType::RParen)?;

        let body = self.parse_block_stmt()?;

        let (start, end) = (ret_ty.span.start, body.span.end);
        Ok(Func {
            name,
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
        let dims = if next_if_match!(self.iter, TokenType::LBracket) {
            expect_token!(self.iter, TokenType::RBracket)?;
            let dims = self.parse_dim()?;
            param_end = dims.span.end;
            Some(dims)
        } else {
            None
        };
        Ok(FuncParam {
            name,
            dims,
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
            let lval = self.parse_l_value()?;
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
        let cond = Rc::new(self.parse_expr()?);
        expect_token!(self.iter, TokenType::RParen)?;

        let then_block = Rc::new(self.parse_block_stmt()?);
        let mut end = then_block.span.end;

        let else_block = if next_if_match!(self.iter, TokenType::ElseKw) {
            let block_stmt = Rc::new(self.parse_block_stmt()?);
            end = block_stmt.span.end;
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
        let cond = Rc::new(self.parse_expr()?);
        expect_token!(self.iter, TokenType::RParen)?;

        let body = Rc::new(self.parse_block_stmt()?);
        let end = body.span.end;

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
            Some(Rc::new(self.parse_expr()?))
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
                        lhs: Rc::new(lhs),
                        rhs: Rc::new(rhs),
                        allow_assign_const: false,
                        span,
                    })
                }
                _ => {
                    let binary_op = op.to_binary_op().unwrap();
                    Expr::Binary(BinaryExpr {
                        lhs: Rc::new(lhs),
                        rhs: Rc::new(rhs),
                        op: binary_op,
                        span,
                    })
                }
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
                expr: Rc::new(expr_item),
                span: Span { start: prec_op.span.start, end },
            });
        }
        Ok(expr_item)
    }

    fn parse_expr_item(&mut self) -> Result<Expr, ParseError> {
        if is_next!(self.iter, TokenType::Ident(_)) {
            let l_value = self.parse_l_value()?;

            if is_next!(self.iter, TokenType::LParen) {
                let call = self.parse_func_call(l_value.name)?;
                Ok(Expr::Call(call))
            } else {
                Ok(Expr::LVal(l_value))
            }
        } else if is_next!(self.iter, TokenType::IntLiteral(_)) {
            let int_literal = expect_token!(self.iter, TokenType::IntLiteral(_))?;
            Ok(Expr::Literal(LiteralExpr {
                kind: LiteralKind::Integer(int_literal.token_type.get_int_literal().unwrap()),
                span: int_literal.span,
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
            params,
            span: Span { start, end },
        })
    }

    fn parse_l_value(&mut self) -> Result<LVal, ParseError> {
        let name = self.parse_ident()?;
        let mut span = name.span;
        let dims = if is_next!(self.iter, TokenType::LBracket) {
            let dims = self.parse_dim()?;
            span.end = dims.span.end;
            Some(dims)
        } else {
            None
        };

        Ok(LVal { name, dims, span })
    }

    fn parse_dim(&mut self) -> Result<Dim, ParseError> {
        let mut span = expect_token!(self.iter, TokenType::LBracket).unwrap().span;
        let dims = parse_separate_match!(self.iter, TokenType::LBracket, {
            let dim = self.parse_expr()?;
            span.end = expect_token!(self.iter, TokenType::RBracket)?.span.end;
            Ok(dim)
        });
        Ok(Dim { dims, span })
    }

    fn parse_ty(&mut self) -> Result<TypeDef, ParseError> {
        let ty_token = expect_token!(self.iter, TokenType::IntTy | TokenType::VoidTy)?;
        Ok(TypeDef {
            ty_kind: ty_token.token_type.to_ty_kind().unwrap(),
            span: ty_token.span,
        })
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let token = expect_token!(self.iter, TokenType::Ident(_))?;
        Ok(Ident {
            span: token.span,
            name: token.token_type.own_ident_name().unwrap(),
        })
    }
}
