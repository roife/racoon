use std::{
    iter::Peekable,
    rc::Rc
};

use super::{
    ast::*,
    err::{ ParseError, ParseError::ExpectedPattern },
    lexer::Lexer,
    span::Span,
    token::{TokenType},
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

macro_rules! while_match_parse {
    ($self:expr, $($sep_pat:pat)|+, $parse:expr) => {{
        let mut v = vec![];
        while next_if_match!($self, $($sep_pat)|+) {
            let val = (|| $parse)()?;
            v.push(val);
        }
        v
    }};
}

macro_rules! until_match_parse {
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

    // CompUnit -> [CompUnit] (Decl | FuncDef)
    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut decls = vec![];
        let mut funcs = vec![];
        while !self.iter.peek().is_none() {
            let is_const = next_if_match!(self.iter, TokenType::ConstKw);
            let ty = self.parse_ty()?;
            let ident = self.parse_ident()?;
            if is_next!(self.iter, TokenType::LParen) {
                let func_stmt = self.parse_func_stmt(ty, ident)?;
                funcs.push(func_stmt);
            } else {
                let decl_stmt = self.parse_decl_stmt(is_const, ty, ident)?;
                decls.push(decl_stmt);
            }
        }
        Ok(Program {
            decls,
            funcs,
        })
    }

    // FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
    // FuncType -> 'void' | 'int'
    // FuncFParams -> FuncFParam { ',' FuncFParam }
    // todo: FuncFParam -> BType Ident ['[' ']' { '[' Exp ']' }]
    fn parse_func_stmt(&mut self, ret_ty: TyDef, name: Ident) -> Result<FuncStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::LParen)?.span.start;
        let params = while_match_parse!(self.iter, TokenType::Comma, {
            let ty = self.parse_ty()?;
            let name = self.parse_ident()?;
                Ok(FuncParam {
                    name,
                    ty
                })
            });
        expect_token!(self.iter, TokenType::RParen)?;
        let body = self.parse_block_stmt()?;
        let end = body.span.end;

        Ok(FuncStmt {
            name,
            params,
            ret_ty,
            body,
            span: Span { start, end }
        })
    }

    // Decl -> ConstDecl | VarDecl
    // ConstDecl -> 'const' BType ConstDef { ',' ConstDef } ';'
    // VarDecl -> BType VarDef { ',' VarDef } ';'
    fn parse_decl_stmt(&mut self, is_const: bool, ty: TyDef, first_ident: Ident) -> Result<DeclStmt, ParseError> {
        todo!()
    }

    // Block -> '{' { BlockItem } '}'
    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::LBrace)?.span.start;
        let stmts = until_match_parse!(self.iter, TokenType::RBrace, self.parse_stmt());
        let end = expect_token!(self.iter, TokenType::RBrace)?.span.end;
        Ok(BlockStmt {
            stmts,
            span: Span { start, end },
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let stmt = if is_next!(self.iter, TokenType::IfKw) {
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

        let (end, else_block) = if next_if_match!(self.iter, TokenType::ElseKw) {
            let else_block = self.parse_block_stmt()?;
            (else_block.span.end, Some(Rc::new(else_block)))
        } else {
            (then_block.span.end, None)
        };

        Ok(IfStmt {
            cond,
            then_block,
            else_block,
            span: Span { start, end }
        })
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
        let start = expect_token!(self.iter, TokenType::IfKw)?.span.start;

        expect_token!(self.iter, TokenType::LParen)?;
        let cond = Rc::new(self.parse_expr()?);
        expect_token!(self.iter, TokenType::RParen)?;

        let body = Rc::new(self.parse_block_stmt()?);

        let end = body.span.end;

        Ok(WhileStmt {
            cond,
            body,
            span: Span { start, end }
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
            span: Span { start, end }
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }

    fn parse_ty(&mut self) -> Result<TyDef, ParseError> {
        let token = expect_token!(self.iter, TokenType::VoidTy | TokenType::IntTy)?;
        Ok(todo!())
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let token = expect_token!(self.iter, TokenType::Ident(_))?;
        Ok(Ident {
            span: token.span,
            name: token.token_type.own_ident_name().unwrap(),
        })
    }
}