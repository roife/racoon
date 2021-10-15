use super::span::{Span, Pos};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LexError {
    pub lex_error_kind: LexErrorKind,
    pub span: Pos,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LexErrorKind {
    IllegalLiteral,
    UnexpectedEOF,
    UnexpectedCharacter(char)
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ParseError {
    pub parse_error_kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ParseErrorKind {
    ExpectedPattern(String)
}