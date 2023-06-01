use enum_as_inner::EnumAsInner;
use crate::compiler::ir::arena::{BBId, GlobalId, InstId, ParamId};
use crate::compiler::ir::value::{constant::Constant, ty::IrTy};

pub trait Value {
    fn get_ty(&self) -> &IrTy;
}

#[derive(Debug, Clone, Eq, Hash, EnumAsInner)]
pub enum Operand {
    Inst(InstId),
    Const(Constant),
    Global(GlobalId),
    Param(ParamId),
    BB(BBId),
}

impl PartialEq for Operand {
    fn eq(&self, other: &Self) -> bool {
        use Operand::{BB, Const, Global, Inst, Param};
        match (self, other) {
            (Inst(x), Inst(y)) => x == y,
            (Const(x), Const(y)) => x == y,
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

impl From<ParamId> for Operand {
    fn from(param_id: ParamId) -> Self {
        Operand::Param(param_id)
    }
}

impl From<i32> for Operand {
    fn from(i: i32) -> Self {
        Operand::Const(Constant::Int(i))
    }
}

impl From<BBId> for Operand {
    fn from(bb_id: BBId) -> Self {
        Operand::BB(bb_id)
    }
}
