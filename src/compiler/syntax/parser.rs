use std::iter::Peekable;

use super::{
    err::ParseError,
    span::Span,
    token::{TokenType, Token},
    lexer::Lexer,
    ast::*
};

macro_rules! is_next {
    ($self:expr, $($pat:pat)|+) => {
        $self.peek().map_or(false, |token| matches!(token, $($pat)|+))
    };
}

pub struct Parser<T>
    where T: Iterator<Item = char>,
{
    lexer_iter: Peekable<Lexer<T>>,
}

impl<T> Parser<T>
    where T: Iterator<Item = char>,
{
    pub fn new(lexer: Lexer<T>) -> Parser<T> {
        Parser {
            lexer_iter: lexer.into_iter().peekable()
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut funcs = vec![];
        let mut decls = vec![];
        loop {
        }
        Ok(Program { decls, funcs })
    }

    fn parse_func_stmt(&mut self) -> Result<FuncStmt, ParseError> {

    }

    fn parse_func_param(&mut self) -> Result<FuncParam, ParseError> {

    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {

    }
}