use super::{ty::Ty, value::Value};

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
}

impl Value for Constant {
    fn get_ty(&self) -> Ty {
        match self {
            Constant::Int(_) => Ty::Int(32),
        }
    }
}