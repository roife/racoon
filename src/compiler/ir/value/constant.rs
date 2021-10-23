use super::{ty::IrTy, value::Value};

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
}

impl Value for Constant {
    fn get_ty(&self) -> IrTy {
        match self {
            Constant::Int(_) => IrTy::Int(32),
        }
    }
}