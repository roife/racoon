use slotmap::SlotMap;

use crate::compiler::ir::arena::{BBId, InstId};
use crate::compiler::ir::value::{basic_block::BasicBlock, inst::Inst};
use crate::compiler::ir::value::inst::InstKind;

use super::{ty::Ty, value::Value};

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub ret_ty: Ty,
    pub is_builtin: bool,
    // pub params:
    pub first_block: Option<BBId>,

    inst_arena: SlotMap<InstId, Inst>,
    bb_arena: SlotMap<BBId, BasicBlock>,
}

impl Value for Func {
    fn get_ty(&self) -> Ty {
        todo!()
    }
}

impl Func {
    pub fn new(name: &str, ret_ty: Ty, is_builtin: bool) -> Func {
        Func {
            name: String::from(name),
            ret_ty,
            is_builtin,
            first_block: None,
            inst_arena: SlotMap::with_key(),
            bb_arena: SlotMap::with_key(),
        }
    }

    pub fn get_inst(&self, inst_id: InstId) -> Option<&Inst> {
        self.inst_arena.get(inst_id)
    }

    pub fn get_bb(&self, bb_id: BBId) -> Option<&BasicBlock> {
        self.bb_arena.get(bb_id)
    }
}

impl Func {
    fn new_inst(&mut self, inst_kind: InstKind, ty: Ty) -> InstId {
        self.inst_arena.insert(Inst {
            kind: inst_kind,
            ty,
            bb: Default::default(),
            prev: None,
            next: None
        })
    }

    // pub fn put_inst_after_current_place(&self, inst_id: InstId, )
}
