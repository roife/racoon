use crate::compiler::ir::value::ty::IrTy;

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch { expected: String, found: IrTy },
    UnknownName(String),
    DuplicateName(String),
    WrongParamLength { expected: usize, found: usize },
    ExpectedFunction(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    NotConstant,
    None
}
