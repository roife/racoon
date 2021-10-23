use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, EnumAsInner)]
pub enum IrTy {
    Void,
    Func(Box<FuncTy>),
    Int(usize),
    Ptr(Box<IrTy>),
    Label,
    Array(Box<IrTy>),
}

#[derive(Debug, Clone)]
pub struct FuncTy {
    pub ret_ty: IrTy,
    pub params_ty: Vec<IrTy>,
}

impl IrTy {
    pub fn bool() -> IrTy {
        IrTy::Int(1)
    }

    pub fn int() -> IrTy {
        IrTy::Int(32)
    }

    pub fn func_of(ret_ty: IrTy, params: Vec<IrTy>) -> IrTy {
        IrTy::Func(Box::new(FuncTy {
            ret_ty,
            params_ty: params,
        }))
    }

    pub fn ptr_of(ty: IrTy) -> IrTy {
        IrTy::Ptr(Box::new(ty))
    }
}