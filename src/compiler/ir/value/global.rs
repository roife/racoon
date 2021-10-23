use crate::compiler::ir::value::{ty::IrTy, value::Value};

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub ty: IrTy,
    pub name: String,
}

impl Value for GlobalVar {
    fn get_ty(&self) -> IrTy {
        self.ty.clone()
    }
}
