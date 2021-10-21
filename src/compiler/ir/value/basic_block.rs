use super::super::arena::{BBId, InstId, ArenaItem};

pub type BBItem = ArenaItem<BBId, BBInfo>;

#[derive(Debug, Clone)]
pub struct BBInfo {
    pub insts_head: Option<InstId>,
    pub insts_tail: Option<InstId>,
}
