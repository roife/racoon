use crate::compiler::ir::arena::{GlobalId, InstId};
use crate::compiler::ir::value::{constant::Constant, ty::IrTy};

pub trait Value {
    fn get_ty(&self) -> IrTy;
}

#[derive(Debug, Clone)]
pub enum Operand {
    Inst(InstId),
    Constant(Constant),
    Global(GlobalId),
}

impl From<InstId> for Operand {
    fn from(inst_id: InstId) -> Self {
        Operand::Inst(inst_id)
    }
}

impl From<i32> for Operand {
    fn from(i: i32) -> Self {
        Operand::Constant(Constant::Int(i))
    }
}
