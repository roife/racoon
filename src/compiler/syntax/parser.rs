use std::iter::Peekable;

use super::{
    err::ParseError,
    span::Span,
    token::{TokenType, Token},
    lexer::Lexer,
    ast::*
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

macro_rules! is_next {
    ($self:expr, $($pat:pat)|+) => {
        $self.peek().map_or(false, |token| matches!(token, $($pat)|+))
    };
}

pub struct Parser<T>
    where T: Iterator<Item = char>,
{
    iter: Peekable<Lexer<T>>,
}

impl<T> Parser<T>
    where T: Iterator<Item = char>,
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
        loop {
        }
//        Ok(Program { decls, funcs })
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let token = expect_token!(self.iter, TokenType::Ident(_))?;
        Ok(Ident {
            span: token.span,
            name: token.token_type.own_ident_name().unwrap()
        })
    }

}