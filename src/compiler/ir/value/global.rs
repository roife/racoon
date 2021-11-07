use crate::compiler::intrusive_linkedlist::IntrusiveLinkedListItem;
use crate::compiler::ir::{
    arena::GlobalId,
    value::{ty::IrTy, value::Value, constant::Constant}
};

#[derive(Debug, Clone)]
pub struct Global {
    pub ty: IrTy,
    pub name: String,
    pub init_val: Constant,

    pub prev: Option<GlobalId>,
    pub next: Option<GlobalId>,
}

impl Global {
    pub fn new(ty: IrTy, name: &str, init_val: Constant) -> Global {
        Global {
            ty,
            name: String::from(name),
            init_val,
            prev: None,
            next: None
        }
    }
}

impl Value for Global {
    fn get_ty(&self) -> &IrTy {
        &self.ty
    }
}

impl IntrusiveLinkedListItem for Global {
    type Key = GlobalId;

    fn next(&self) -> Option<Self::Key> {
        self.next
    }

    fn set_next(&mut self, key: Option<Self::Key>) {
        self.next = key
    }

    fn prev(&self) -> Option<Self::Key> {
        self.prev
    }

    fn set_prev(&mut self, key: Option<Self::Key>) {
        self.prev = key
    }
}