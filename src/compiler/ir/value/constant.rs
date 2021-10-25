use super::{ty::IrTy, value::Value};

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
    Array(usize, Box<Constant>)
}

impl Value for Constant {
    fn get_ty(&self) -> IrTy {
        match self {
            Constant::Int(_) => IrTy::Int(32),
            Constant::Array(size, sub_item) => IrTy::Array(*size, Box::new(sub_item.get_ty()))
        }
    }
}