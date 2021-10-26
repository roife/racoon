use std::fmt::{Display, Formatter};

use crate::compiler::ir::value::{ty::IrTy, value::Value};
use crate::compiler::ir::value::constant::Constant;

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub ty: IrTy,
    pub name: String,
    pub init_val: Constant,
}

impl GlobalVar {
    pub fn new(ty: IrTy, name: &str, init_val: Constant) -> GlobalVar {
        GlobalVar {
            ty,
            name: String::from(name),
            init_val,
        }
    }
}

impl Value for GlobalVar {
    fn get_ty(&self) -> IrTy {
        self.ty.clone()
    }
}

impl Display for GlobalVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @{}", self.ty, self.name)
    }
}