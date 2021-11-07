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

impl Default for IrTy {
    fn default() -> Self {
        IrTy::Void
    }
}

impl PartialEq<Self> for IrTy {
    fn eq(&self, other: &Self) -> bool {
        use IrTy::*;
        match (self, other) {
            (Void, Void) | (Label, Label) => true,
            (Int(x), Int(y)) => x == y,
            (Array(siz1, ty1), Array(siz2, ty2)) => siz1 == siz2 && ty1 == ty2,
            (Ptr(x) | Array(_, x), Ptr(y) | Array(_, y)) => x.as_ref() == y.as_ref(),
            (Func(x), Func(y)) => x.as_ref() == y.as_ref(),
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

    pub fn ptr_of(ty: &IrTy) -> IrTy {
        IrTy::Ptr(Box::new(ty.clone()))
    }

    pub fn deptr_of(ty: &IrTy) -> Option<IrTy> {
        match ty {
            IrTy::Ptr(x) => Some(x.as_ref().clone()),
            _ => None
        }
    }
}

impl Display for IrTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IrTy::Void => String::from("void"),
            IrTy::Int(x) => format!("i{}", x),
            IrTy::Ptr(t) => format!("{}*", t),
            IrTy::Label => String::from("label"),
            IrTy::Array(dim_size, elem_ty) => format!("[{} x {}]", dim_size, elem_ty),
            _ => unreachable!()
        };
        write!(f, "{}", s)
    }
}