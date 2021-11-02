use crate::compiler::syntax::ast::AstTy;

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch { expected: String, found: AstTy },
    UnknownName(String),
    DuplicateName(String),
    WrongParamLength { expected: usize, found: usize },
    ExpectedFunction(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    TooMuchElement,
    IllegalArrayDim,
    RequireConstant,
    RequireLValue,
    CannotModifyConstValue(String),
}
