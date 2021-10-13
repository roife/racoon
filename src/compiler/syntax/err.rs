#[derive(Debug, PartialEq, Eq, Clone, Hash, Copy)]
pub enum LexError {
    IllegalLiteral,
    UnexpectedEOF,
    UnexpectedCharacter(char)
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Copy)]
pub enum ParseError {
    None
}