use crate::compiler::ir::arena::{BBId, GlobalId, InstId, ParamId};
use crate::compiler::ir::value::{constant::Constant, ty::IrTy};

pub trait Value {
    fn get_ty(&self) -> IrTy;
}

#[derive(Debug, Clone, Eq, Hash)]
pub enum Operand {
    Inst(InstId),
    Constant(Constant),
    Global(GlobalId),
    Param(ParamId),
    BB(BBId),
}

impl PartialEq for Operand {
    fn eq(&self, other: &Self) -> bool {
        use Operand::*;
        match (self, other) {
            (Inst(x), Inst(y)) => x == y,
            (Constant(x), Constant(y)) => x == y,
            (Global(x), Global(y)) => x == y,
            (Param(x), Param(y)) => x == y,
            (BB(x), BB(y)) => x == y,
            _ => false
        }
    }
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
