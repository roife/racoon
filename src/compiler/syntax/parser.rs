use std::iter::Peekable;

use super::{
    ast::*,
    err::ParseError,
    lexer::Lexer,
    span::Span,
    token::{Token, TokenType},
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
        loop {
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
        todo!()
    }

    // Decl -> ConstDecl | VarDecl
    // ConstDecl -> 'const' BType ConstDef { ',' ConstDef } ';'
    // VarDecl -> BType VarDef { ',' VarDef } ';'
    fn parse_decl_stmt(&mut self, is_const: bool, ty: TyDef, first_ident: Ident) -> Result<DeclStmt, ParseError> {
        todo!()
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