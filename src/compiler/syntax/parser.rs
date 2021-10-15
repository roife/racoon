use std::{
    iter::Peekable,
    rc::Rc
};

use super::{
    ast::*,
    err::{ParseError, ParseErrorKind},
    lexer::Lexer,
    span::Span,
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

macro_rules! parse_while_match {
    ($self:expr, $($sep_pat:pat)|+, $parse:expr) => {{
        let mut v = vec![];
        while next_if_match!($self, $($sep_pat)|+) {
            let val = (|| $parse)()?;
            v.push(val);
        }
        v
    }};
}

macro_rules! parse_until_match {
    ($self:expr, $($term_pat:pat)|+, $parse:expr) => {{
        let mut v = vec![];
        while !is_next!($self, $($term_pat)|+) {
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
        let mut decls = vec![];
        let mut funcs = vec![];
        while self.iter.peek().is_some() {
            let is_const = next_if_match!(self.iter, TokenType::ConstKw);
            let ty = self.parse_ty()?;
            let lval = self.parse_lval()?;

            if is_next!(self.iter, TokenType::LParen) {
                let func_stmt = self.parse_func_stmt(ty, lval.name)?;
                funcs.push(func_stmt);
            } else {
                let mut sub_decls = vec![self.parse_sub_decl(is_const, lval)?];
                sub_decls.append(&mut parse_while_match!(self.iter, TokenType::Comma, {
                    let lval = self.parse_lval()?;
                    self.parse_sub_decl(is_const, lval)
                }));

                let start = ty.span.start;
                let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;

                decls.push(Decl {
                    is_const,
                    ty,
                    sub_decls,
                    span: Span { start, end }
                });
            }
        }
        Ok(Program {
            decls,
            funcs,
        })
    }

    fn parse_func_stmt(&mut self, ret_ty: TypeDef, name: Ident) -> Result<Func, ParseError> {
        expect_token!(self.iter, TokenType::LParen)?;
        let params = parse_while_match!(self.iter, TokenType::Comma, self.parse_func_param());

        expect_token!(self.iter, TokenType::RParen)?;
        let body = self.parse_block_stmt()?;

        let start = ret_ty.span.start;
        let end = body.span.end;

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
                end: param_end
            },
        })

    }

    fn parse_sub_decl(&mut self, is_const: bool, lval: LVal) -> Result<SubDecl, ParseError> {
        let start = lval.name.span.start;
        let mut end = lval.name.span.end;

        let init_val = if is_const || is_next!(self.iter, TokenType::Assign) {
            expect_token!(self.iter, TokenType::Assign)?;
            let init_val = self.parse_init_val()?;
            end = init_val.span().end;
            Some(Rc::new(init_val))
        } else {
            None
        };
        Ok(SubDecl {
            name: lval,
            init_val,
            span: Span { start, end },
        })
    }

    fn parse_init_val(&mut self) -> Result<InitVal, ParseError> {
        let init_val = if next_if_match!(self.iter, TokenType::LBrace) {
            let init_val = parse_while_match!(self.iter, TokenType::Comma, {
                let sub_init_val = self.parse_init_val()?;
                Ok(Rc::new(sub_init_val))
            });
            InitVal::ArrayVal(init_val)
        } else {
            InitVal::Expr(self.parse_expr()?)
        };
        Ok(init_val)
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::LBrace)?.span.start;
        let block_items = parse_until_match!(self.iter, TokenType::RBrace, {
            if is_next!(self.iter, TokenType::IntTy | TokenType::VoidTy | TokenType::ConstKw) {
                let decl = self.parse_decl_stmt()?;
                Ok(BlockItem::Decl(decl))
            } else {
                let stmt = self.parse_stmt()?;
                Ok(BlockItem::Stmt(stmt))
            }
        });
        let end = expect_token!(self.iter, TokenType::RBrace)?.span.end;
        Ok(BlockStmt {
            block_items,
            span: Span { start, end },
        })
    }

    fn parse_decl_stmt(&mut self) -> Result<Decl, ParseError> {
        let is_const = next_if_match!(self.iter, TokenType::ConstKw);
        let ty = self.parse_ty()?;
        let sub_decls = parse_while_match!(self.iter, TokenType::Comma, {
            let lval = self.parse_lval()?;
            self.parse_sub_decl(is_const, lval)
        });
        println!("{:?}", self.iter.peek());

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
            let else_block = self.parse_block_stmt()?;
            end = else_block.span.end;
            Some(Rc::new(else_block))
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

            while let Some(next_op_token) = self.iter.next_if(|next_token| {
                let next_op = &next_token.token_type;
                next_op.is_binary_op()
                    && ((next_op.prec() > op.prec() && next_op.is_left_assoc())
                    || (next_op.prec() == op.prec() && !next_op.is_left_assoc()))
            }) {
                let op = next_op_token.token_type;
                let op_precedence = op.prec();
                rhs = self.parse_expr_opg(rhs, op_precedence)?;
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
        while is_next!(self.iter, TokenType::Plus | TokenType::Minus) {
            pre_op_tokens.push(self.iter.next().unwrap())
        }

        let mut expr_item = self.parse_expr_item()?;
        let end = expr_item.span().end;
        for prec_op in pre_op_tokens.drain(..).rev() {
            let op = match prec_op.token_type {
                TokenType::Plus => UnaryOp::Pos,
                TokenType::Minus => UnaryOp::Neg,
                _ => unreachable!(),
            };
            expr_item = Expr::Unary(UnaryExpr {
                op,
                expr: Rc::new(expr_item),
                span: Span { start: prec_op.span.start, end }
            });
        }
        Ok(expr_item)
    }

    fn parse_expr_item(&mut self) -> Result<Expr, ParseError> {
        if is_next!(self.iter, TokenType::Ident(_)) {
            let lval = self.parse_lval()?;

            if is_next!(self.iter, TokenType::LParen) {
                let call = self.parse_func_call(lval.name)?;
                Ok(Expr::Call(call))
            } else {
                Ok(Expr::LVal(lval))
            }
        } else if is_next!(self.iter, TokenType::IntLiteral(_)) {
            let int_literal = expect_token!(self.iter, TokenType::IntLiteral(_))?;
            Ok(Expr::Literal(LiteralExpr {
                kind: LiteralKind::Integer(int_literal.token_type.get_int_literal().unwrap()),
                span: int_literal.span
            }))
        } else if is_next!(self.iter, TokenType::LParen) {
            expect_token!(self.iter, TokenType::LParen)?;
            let expr = self.parse_expr()?;
            expect_token!(self.iter, TokenType::RParen)?;
            Ok(expr)
        } else {
            Err(ParseError {
                parse_error_kind: ParseErrorKind::ExpectedPattern(
                    "Literal or Identifier or Function Call or Parenthesis".into()
                ),
                span: self.iter.peek().map_or(Span::MAX, |x| x.span),
            })
        }
    }

    fn parse_func_call(&mut self, func: Ident) -> Result<CallExpr, ParseError> {
        let start = func.span.start;
        expect_token!(self.iter, TokenType::LParen)?;
        let params = parse_while_match!(
            self.iter,
            TokenType::Comma,
            self.parse_expr()
        );
        let end = expect_token!(self.iter, TokenType::RParen)?.span.end;

        Ok(CallExpr {
            func,
            params,
            span: Span { start, end },
        })
    }

    fn parse_lval(&mut self) -> Result<LVal, ParseError> {
        let name = self.parse_ident()?;
        let start = name.span.start;
        let mut end = name.span.end;
        let dims = if is_next!(self.iter, TokenType::LBracket) {
            let dims = self.parse_dim()?;
            end = dims.span.end;
            Some(dims)
        } else {
            None
        };

        Ok(LVal {
            name,
            dims,
            span: Span { start, end },
        })
    }

    fn parse_dim(&mut self) -> Result<Dim, ParseError> {
        let mut span = self.iter.peek().unwrap().span;
        let dims = parse_while_match!(self.iter, TokenType::LBracket, {
            let dim = self.parse_expr()?;
            span.end = expect_token!(self.iter, TokenType::RBracket)?.span.end;
            Ok(Rc::new(dim))
        });
        Ok(Dim {
            dims,
            span,
        })
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
