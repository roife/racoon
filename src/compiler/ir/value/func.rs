use slotmap::SlotMap;

use crate::compiler::ir::arena::{BBId, InstId};
use crate::compiler::ir::value::{basic_block::BasicBlock, inst::{Inst, InstKind}, ty::IrTy, value::Value};
use crate::compiler::ir::value::inst::BranchInst;

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub ret_ty: IrTy,
    pub is_builtin: bool,
    // pub params:
    pub first_block: Option<BBId>,

    inst_arena: SlotMap<InstId, Inst>,
    bb_arena: SlotMap<BBId, BasicBlock>,
}

impl Value for Func {
    fn get_ty(&self) -> IrTy {
        todo!()
    }
}

impl Func {
    pub fn new(name: &str, ret_ty: IrTy, is_builtin: bool) -> Func {
        Func {
            name: String::from(name),
            ret_ty,
            is_builtin,
            first_block: None,
            inst_arena: SlotMap::with_key(),
            bb_arena: SlotMap::with_key(),
        }
    }
}

impl Func {
    pub fn get_inst(&self, inst_id: InstId) -> Option<&Inst> {
        self.inst_arena.get(inst_id)
    }

    pub fn get_inst_mut(&mut self, inst_id: InstId) -> Option<&mut Inst> {
        self.inst_arena.get_mut(inst_id)
    }

    fn new_inst(&mut self, inst_kind: InstKind, ty: IrTy, bb: BBId) -> InstId {
        self.inst_arena.insert(Inst {
            kind: inst_kind,
            ty,
            bb,
            prev: None,
            next: None
        })
    }

    pub fn set_inst_before_cur(&mut self, before: InstId, cur_inst: InstId) {
        todo!()
    }

    pub fn set_inst_after_cur(&mut self, after: InstId, cur_inst: InstId) {
        todo!()
    }

    pub fn build_inst_after_cur(&mut self, inst_kind: InstKind, ty: IrTy, cur_inst: InstId) -> InstId {
        let bb = self.get_inst(cur_inst).unwrap().bb;
        let new_inst = self.new_inst(inst_kind, ty, bb);
        self.set_inst_after_cur(new_inst, cur_inst);
        new_inst
    }

    pub fn build_inst_before_cur(&mut self, inst_kind: InstKind, ty: IrTy, cur_inst: InstId) -> InstId {
        let bb = self.get_inst(cur_inst).unwrap().bb;
        let new_inst = self.new_inst(inst_kind, ty, bb);
        self.set_inst_before_cur(new_inst, cur_inst);
        new_inst
    }

    pub fn build_inst_at_end(&mut self, inst_kind: InstKind, ty: IrTy, bb: BBId) -> InstId {
        let new_inst = self.new_inst(inst_kind, ty, bb);
        let bb = self.get_bb_mut(bb).unwrap();
        let old_tail = bb.insts_tail.replace(new_inst);
        if bb.insts_head.is_none() {
            bb.insts_head = Some(new_inst);
        }
        if let Some(old_tail) = old_tail {
            self.set_inst_before_cur(old_tail, new_inst);
        }
        new_inst
    }

    pub fn build_inst_at_start(&mut self, inst_kind: InstKind, ty: IrTy, bb: BBId) -> InstId {
        let new_inst = self.new_inst(inst_kind, ty, bb);
        let bb = self.get_bb_mut(bb).unwrap();
        let old_head = bb.insts_head.replace(new_inst);
        if bb.insts_tail.is_none() {
            bb.insts_tail = Some(new_inst);
        }
        if let Some(old_head) = old_head {
            self.set_inst_after_cur(old_head, new_inst);
        }
        new_inst
    }
}

impl Func {
    pub fn get_bb(&self, bb_id: BBId) -> Option<&BasicBlock> {
        self.bb_arena.get(bb_id)
    }

    pub fn get_bb_mut(&mut self, bb_id: BBId) -> Option<&mut BasicBlock> {
        self.bb_arena.get_mut(bb_id)
    }

    fn new_bb(&mut self) -> BBId {
        self.bb_arena.insert(BasicBlock::default())
    }

    pub fn set_bb_before_cur(&mut self, before: BBId, cur_bb: BBId) {
        todo!()
    }

    pub fn set_bb_after_cur(&mut self, after: BBId, cur_bb: BBId) {
        todo!()
    }

    pub fn build_bb_after_cur(&mut self, cur_bb: BBId) -> BBId {
        let new_bb = self.new_bb();
        self.set_bb_after_cur(new_bb, cur_bb);
        new_bb
    }

    pub fn build_bb_before_cur(&mut self, cur_bb: BBId) -> BBId {
        let new_bb = self.new_bb();
        self.set_bb_before_cur(new_bb, cur_bb);
        new_bb
    }
}