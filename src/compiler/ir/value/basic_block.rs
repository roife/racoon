use crate::compiler::intrusive_linkedlist::IntrusiveLinkedListItem;
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

impl IntrusiveLinkedListItem for BasicBlock {
    type Key = BBId;

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
