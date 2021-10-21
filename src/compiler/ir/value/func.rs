use slotmap::SlotMap;
use super::{
    super::arena::{BBId, InstId},
    ty::Ty,
    basic_block::BasicBlock,
    inst::Inst,
};

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub ty: Ty,
    pub is_builtin: bool,
    // pub params: todo

    pub first_block: Option<BBId>,

    instructions_arena: SlotMap<InstId, Inst>,
    basic_block_arena: SlotMap<BBId, BasicBlock>,
}

impl Func {
    pub fn new(name: &str, ty: Ty, is_builtin: bool) -> Func {
        Func {
            name: String::from(name),
            ty,
            is_builtin,
            instructions_arena: SlotMap::with_key(),
            basic_block_arena: SlotMap::with_key(),
            first_block: None,
        }
    }
}