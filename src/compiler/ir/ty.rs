use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Ty {
    Void,
    Func(FuncTy),
    Int(usize),
    Ptr(Rc<Ty>),
    Label,
    Array(Rc<Ty>),
}

#[derive(Debug, Clone)]
pub struct FuncTy {
    pub ret_ty: Rc<Ty>,
    pub params: Vec<Rc<Ty>>,
}