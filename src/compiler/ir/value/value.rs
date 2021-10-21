use crate::compiler::ir::arena::{InstId};
use crate::compiler::ir::value::{ty::Ty, constant::Constant};

pub trait Value {
    fn get_ty(&self) -> Ty;
}

#[derive(Debug, Clone)]
pub enum Operand {
    Inst(InstId),
    Constant(Constant)
}