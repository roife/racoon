use std::fmt::{Debug, Display, Formatter};

use super::{ty::IrTy, value::Value};

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
    // empty vec represents zero initializer
    Array(IrTy, Vec<Box<Constant>>),
}

impl Constant {
    pub fn build_zero(ty: IrTy) -> Constant {
        match ty {
            IrTy::Int(_) => Self::Int(0),
            IrTy::Array(siz, ty) => Self::Array(*ty, vec![]),
            _ => unreachable!()
        }
    }
}

impl Value for Constant {
    fn get_ty(&self) -> IrTy {
        match self {
            Constant::Int(_) => IrTy::Int(32),
            Constant::Array(ty, _) => ty.clone(),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(x) => write!(f, "i32 {}", x),
            Constant::Array(ty, data) => {
                todo!();
                if data.is_empty() {
                    write!(f, "[{} zeroinitializer]", ty)
                } else {
                    let data_str = data.iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "[{} {}]", ty, data_str)
                }
            }
        }
    }
}