use std::fmt::{Debug, Formatter};
use std::rc::Rc;

use enum_as_inner::EnumAsInner;

use crate::compiler::span::Span;

use super::err::LexError;

#[derive(Debug, Eq, PartialEq, Hash, Clone, EnumAsInner)]
pub enum TokenType {
    Ident(String),
    ConstKw,
    IntTy, VoidTy,
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
    Err(Rc<LexError>)
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