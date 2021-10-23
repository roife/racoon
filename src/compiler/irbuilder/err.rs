use crate::compiler::ir::value::ty::IrTy;

#[derive(Debug)]
pub enum Error {
    TypeMismatch { expected: IrTy, found: IrTy },
    UnknownName(String),
    DuplicateName(String),
    WrongParamLength { expected: usize, found: usize },
    ExpectedFunction(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    None
}
