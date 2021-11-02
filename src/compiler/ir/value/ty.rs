use std::fmt::{Display, Formatter};

use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, Eq, Hash, EnumAsInner)]
pub enum IrTy {
    Void,
    Int(usize),
    Func(Box<FuncTy>),
    Ptr(Box<IrTy>),
    Label,
    Array(usize, Box<IrTy>),
}

impl PartialEq<Self> for IrTy {
    fn eq(&self, other: &Self) -> bool {
        use IrTy::*;
        match (self, other) {
            (Void, Void) => true,
            (Int(x), Int(y)) => x == y,
            (Ptr(x), Ptr(y)) => x.as_ref() == y.as_ref(),
            (Func(x), Func(y)) => x.as_ref() == y.as_ref(),
            (Label, Label) => true,
            (Array(siz1, ty1), Array(siz2, ty2)) => siz1 == siz2 && ty1 == ty2,
            _ => false,
        }
    }
}

#[derive(Debug, Eq, Clone, Hash)]
pub struct FuncTy {
    pub ret_ty: IrTy,
    pub params_ty: Vec<IrTy>,
}

impl PartialEq<Self> for FuncTy {
    fn eq(&self, other: &Self) -> bool {
        self.ret_ty == other.ret_ty &&
            self.params_ty.iter()
                .zip(&other.params_ty)
                .all(|(x, y)| x == y)
    }
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

impl Display for IrTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IrTy::Void => String::from("void"),
            IrTy::Func(_) => format!("todo"),
            IrTy::Int(x) => format!("i{}", x),
            IrTy::Ptr(t) => format!("{}*", t),
            IrTy::Label => format!("todo"),
            IrTy::Array(dim_size, elem_ty) => format!("[{} x {}]", dim_size, elem_ty),
        };
        write!(f, "{}", s)
    }
}