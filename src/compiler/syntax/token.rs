use std::fmt::{Debug, Formatter};
use super::{
    span::Span,
    err::LexError
};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum TokenType {
    Ident(String),
    ConstKw,
    IntType, VoidType,
    IntLiteral(i32),
    WhileKw, BreakKw, ContinueKw,
    IfKw, ElseKw,
    Not, And, Or,
    ReturnKw,
    Plus, Minus, Mul, Div, Mod,
    Lt, Le, Gt, Ge, Eq, Ne,
    Assign,
    Semicolon, Comma,
    LParen, RParen, LBracket, RBracket, LBrace, RBrace,
    Comment(String),
    Err(LexError)
}

impl TokenType {
    pub fn own_ident_name(self) -> Option<String> {
        match self {
            TokenType::Ident(name) => Some(name.clone()),
            _ => None,
        }
    }

    pub fn get_int_literal(&self) -> Option<i32> {
        match self {
            TokenType::IntLiteral(i) => Some(*i),
            _ => None
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_type)
    }
}