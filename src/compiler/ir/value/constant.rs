use std::fmt::{Debug, Display, Formatter};

use super::{ty::IrTy, value::Value};
use enum_as_inner::EnumAsInner;
use itertools::Itertools;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Constant {
    Int(i32),
    // empty vec represents zero initializer
    Array(IrTy, Vec<Constant>),
}

impl Constant {
    pub fn build_zero(ty: IrTy) -> Constant {
        match ty {
            IrTy::Int(_) => Self::Int(0),
            ty @ IrTy::Array(_, _) => Self::Array(ty, vec![]),
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
            Constant::Array(ty @ IrTy::Array(siz, elem_ty),
                            vals) => {
                if vals.is_empty() {
                    write!(f, "{} zeroinitializer", ty)
                } else {
                    let mut data_str: String = vals.iter().join(", ");

                    if vals.len() < *siz {
                        let zeros = Self::build_zero(elem_ty.as_ref().clone());
                        let zeros_str = format!(", {}", zeros).repeat(*siz - vals.len());
                        data_str.push_str(&zeros_str);
                    }

                    write!(f, "{} [{}]", ty, data_str)
                }
            }
            _ => unreachable!()
        }
    }
}

impl From<i32> for Constant {
    fn from(x: i32) -> Self {
        Self::Int(x)
    }
}