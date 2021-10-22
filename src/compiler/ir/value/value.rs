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

impl Value for Operand {
    fn get_ty(&self) -> Ty {
        match self {
            Operand::Inst(i) => todo!(),
            Operand::Constant(c) => c.get_ty()
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
