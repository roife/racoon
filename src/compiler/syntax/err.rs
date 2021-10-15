#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LexError {
    IllegalLiteral,
    UnexpectedEOF,
    UnexpectedCharacter(char)
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ParseError {
    ExpectedPattern(String)
}