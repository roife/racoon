use crate::compiler::ir::value::ty::Ty;
use crate::compiler::ir::value::value::Value;

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub ty: Ty,
    pub name: String,
}

impl Value for GlobalVar {
    fn get_ty(&self) -> Ty {
        self.ty.clone()
    }
}
