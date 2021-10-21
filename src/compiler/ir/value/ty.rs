use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Ty {
    Void,
    Func(Box<FuncTy>),
    Int(usize),
    Ptr(Box<Ty>),
    Label,
    Array(Box<Ty>),
}

#[derive(Debug, Clone)]
pub struct FuncTy {
    pub ret_ty: Ty,
    pub params_ty: Vec<Ty>,
}

impl Ty {
    pub fn func_of(ret_ty: Ty, params: Vec<Ty>) -> Ty {
        Ty::Func(Box::new(FuncTy {
            ret_ty,
            params_ty: params,
        }))
    }

    pub fn ptr_of(ty: Ty) -> Ty {
        Ty::Ptr(Box::new(ty))
    }
}