use super::{
    super::arena::{BBId, InstId},
    super::intrusive_linkedlist::IntrusiveListItem,
    ty::Ty,
    inst::Inst,
};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub insts_head: Option<InstId>,
    pub insts_tail: Option<InstId>,

    pub prev: Option<BBId>,
    pub next: Option<BBId>,
}

impl IntrusiveListItem for BasicBlock {
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

