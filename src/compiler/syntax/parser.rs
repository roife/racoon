use std::{
    iter::Peekable,
    rc::Rc
};

use super::{
    ast::*,
    err::{ParseError, ParseError::ExpectedPattern},
    lexer::Lexer,
    span::Span,
    token::TokenType,
};

macro_rules! expect_token {
    ($self:expr, $($pat:pat)|+) => {
        $self.next_if(|token| matches!(token.token_type, $($pat)|+))
        .map_or(
            Err(ParseError::ExpectedPattern(stringify!($($pat)|+).to_owned())),
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
        while !self.iter.peek().is_none() {
            let is_const = next_if_match!(self.iter, TokenType::ConstKw);
            let ty = self.parse_ty()?;
            let lval = self.parse_lval()?;
            if is_next!(self.iter, TokenType::LParen) {
                let func_stmt = self.parse_func_stmt(ty, lval.name)?;
                funcs.push(func_stmt);
            } else {
                decls.push(self.parse_decl_stmt(is_const, ty.clone(), lval)?);
                let mut decl = parse_while_match!(self.iter, TokenType::Comma, {
                    let lval = self.parse_lval()?;
                    self.parse_decl_stmt(is_const, ty.clone(), lval)
                });
                decls.append(&mut decl);
            }
        }
        Ok(Program {
            decls,
            funcs,
        })
    }
    
    fn parse_func_stmt(&mut self, ret_ty: TypeDef, name: Ident) -> Result<Func, ParseError> {
        let start = expect_token!(self.iter, TokenType::LParen)?.span.start;
        let params = parse_while_match!(self.iter, TokenType::Comma, {
            let ty = self.parse_ty()?;
            let name = self.parse_ident()?;
            let dims = if is_next!(self.iter, TokenType::LBracket) {
                expect_token!(self.iter, TokenType::LBracket)?;
                expect_token!(self.iter, TokenType::RBracket)?;
                let dims = parse_while_match!(self.iter, TokenType::LBracket, {
                    let dim = self.parse_expr()?;
                    expect_token!(self.iter, TokenType::RBracket)?;
                    Ok(Rc::new(dim))
                });
                Some(dims)
            } else {
                None
            };
            Ok(FuncParam {
                name,
                dims,
                ty
            })
        });
        expect_token!(self.iter, TokenType::RParen)?;
        let body = self.parse_block_stmt()?;
        let end = body.span.end;

        Ok(Func {
            name,
            params,
            ret_ty,
            body,
            span: Span { start, end },
        })
    }

    fn parse_decl_stmt(&mut self, is_const: bool, ty: TypeDef, lval: LVal) -> Result<Decl, ParseError> {
        let start = lval.name.span.start;
        let mut end = lval.name.span.end;

        let init_val = if is_const || next_if_match!(self.iter, TokenType::Assign) {
            let init_val = self.parse_init_val()?;
            end = init_val.span().end;
            Some(Rc::new(init_val))
        } else {
            None
        };

        Ok(Decl {
            is_const,
            name: lval.name,
            ty,
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
            InitVal::InitVal(init_val)
        } else {
            InitVal::Expr(self.parse_expr()?)
        };
        Ok(init_val)
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::LBrace)?.span.start;
        let block_items = parse_until_match!(self.iter, TokenType::RBrace, {
            todo!();
            Err(ParseError::ExpectedPattern(String::from("123")))
        });
        let end = expect_token!(self.iter, TokenType::RBrace)?.span.end;
        Ok(BlockStmt {
            block_items,
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
            let start = expect_token!(self.iter, TokenType::BreakKw)?.span.start;
            let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;
            Stmt::Break(Span { start, end })
        } else if is_next!(self.iter, TokenType::ContinueKw) {
            let start = expect_token!(self.iter, TokenType::ContinueKw)?.span.start;
            let end = expect_token!(self.iter, TokenType::Semicolon)?.span.end;
            Stmt::Continue(Span { start, end })
        } else if is_next!(self.iter, TokenType::ReturnKw) {
            Stmt::Return(self.parse_return_stmt()?)
        } else if is_next!(self.iter, TokenType::Semicolon) {
            let span = expect_token!(self.iter, TokenType::Semicolon)?.span;
            Stmt::Empty(span)
        } else {
            return Err(ParseError::ExpectedPattern(String::from("(Stmt)")));
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

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_lval(&mut self) -> Result<LVal, ParseError> {
        let name = self.parse_ident()?;
        let start = name.span.start;
        let mut end = name.span.end;
        let dims = if is_next!(self.iter, TokenType::LBracket) {
            let dims = parse_while_match!(self.iter, TokenType::LBracket, {
                let dim = self.parse_expr()?;
                end = expect_token!(self.iter, TokenType::RBracket)?.span.end;
                Ok(Rc::new(dim))
            });
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

    fn parse_ty(&mut self) -> Result<TypeDef, ParseError> {
        let ident = self.parse_ident()?;
        let span = ident.span.clone();
        Ok(TypeDef {
            ty_name: ident,
            span,
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