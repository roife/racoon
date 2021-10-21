use super::{ty::Ty, value::Value};
use super::super::arena::{BBId, InstId};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub insts_head: Option<InstId>,
    pub insts_tail: Option<InstId>,

    pub prev: Option<BBId>,
    pub next: Option<BBId>,
}

impl Value for BasicBlock {
    fn get_ty(&self) -> Ty {
        Ty::Label
    }
}