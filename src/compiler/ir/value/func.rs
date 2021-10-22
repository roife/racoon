use slotmap::SlotMap;
use crate::compiler::intrusive_linkedlist::IntrusiveLinkedList;

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

    pub fn get_inst_mut(&mut self, inst_id: InstId) -> Option<&mut Inst> {
        self.inst_arena.get_mut(inst_id)
    }

    pub fn get_bb(&self, bb_id: BBId) -> Option<&BasicBlock> {
        self.bb_arena.get(bb_id)
    }

    pub fn get_bb_mut(&mut self, bb_id: BBId) -> Option<&mut BasicBlock> {
        self.bb_arena.get_mut(bb_id)
    }
}

impl Func {
    fn new_inst(&mut self, inst_kind: InstKind, ty: Ty, bb: BBId) -> InstId {
        self.inst_arena.insert(Inst {
            kind: inst_kind,
            ty,
            bb,
            prev: None,
            next: None
        })
    }

    pub fn inst_set_before_cur(&mut self, before: InstId, cur_inst: InstId) {
        todo!()
    }

    pub fn inst_set_after_cur(&mut self, after: InstId, cur_inst: InstId) {
        todo!()
    }

    pub fn inst_new_after_cur(&mut self, inst_kind: InstKind, ty: Ty, cur_inst: InstId) -> InstId {
        let bb = self.get_inst(cur_inst).unwrap().bb;
        let id = self.new_inst(inst_kind, ty, bb);
        self.inst_set_after_cur(id, cur_inst);
        id
    }

    pub fn inst_new_before_cur(&mut self, inst_kind: InstKind, ty: Ty, cur_inst: InstId) -> InstId {
        let bb = self.get_inst(cur_inst).unwrap().bb;
        let id = self.new_inst(inst_kind, ty, bb);
        self.inst_set_before_cur(id, cur_inst);
        id
    }

    pub fn inst_new_at_end_of(&mut self, inst_kind: InstKind, ty: Ty, bb: BBId) -> InstId {
        todo!()
    }

    pub fn inst_new_at_start_of(&mut self, inst_kind: InstKind, ty: Ty, bb: BBId) -> InstId {
        todo!()
    }
}
