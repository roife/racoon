use slotmap::SlotMap;

use crate::compiler::intrusive_linkedlist::{IntrusiveLinkedList, IntrusiveLinkedListItem};
use crate::compiler::ir::arena::{BBId, FuncId, InstId, ParamId};
use crate::compiler::ir::value::{basic_block::*, inst::*, ty::*, value::*};

#[derive(Debug)]
pub struct IrFuncParam {
    pub ty: IrTy,
    pub pos: usize,
}

#[derive(Debug, Default)]
pub struct IrFunc {
    pub name: String,
    pub ret_ty: IrTy,
    pub is_builtin: bool,
    pub params: Vec<ParamId>,
    ty: IrTy,

    pub first_block: Option<BBId>,

    pub param_arena: SlotMap<ParamId, IrFuncParam>,
    pub inst_arena: SlotMap<InstId, Inst>,
    pub bb_arena: SlotMap<BBId, BasicBlock>,

    pub prev: Option<FuncId>,
    pub next: Option<FuncId>,
}

impl Value for IrFunc {
    fn get_ty(&self) -> &IrTy {
        &self.ty
    }
}

impl IntrusiveLinkedListItem for IrFunc {
    type Key = FuncId;

    fn next(&self) -> Option<Self::Key> {
        self.next
    }

    fn set_next(&mut self, key: Option<Self::Key>) {
        self.next = key;
    }

    fn prev(&self) -> Option<Self::Key> {
        self.prev
    }

    fn set_prev(&mut self, key: Option<Self::Key>) {
        self.prev = key;
    }
}

impl IrFunc {
    pub fn new(name: &str, ret_ty: IrTy, is_builtin: bool) -> IrFunc {
        IrFunc {
            name: String::from(name),
            ret_ty: ret_ty.clone(),
            is_builtin,
            params: vec![],
            first_block: None,
            ty: IrTy::func_of(ret_ty, vec![]),

            param_arena: SlotMap::with_key(),
            inst_arena: SlotMap::with_key(),
            bb_arena: SlotMap::with_key(),

            prev: None,
            next: None
        }
    }

    pub fn build_func_param(&mut self, ty: IrTy) -> ParamId {
        self.ty.as_func_mut().unwrap().params_ty.push(ty.clone());
        let pos = self.params.len();
        let param = IrFuncParam { ty, pos };
        let param_id = self.param_arena.insert(param);
        self.params.push(param_id);
        param_id
    }
}

impl IrFunc {
    pub fn get_inst(&self, inst_id: InstId) -> Option<&Inst> {
        self.inst_arena.get(inst_id)
    }

    pub fn get_param(&self, param_id: ParamId) -> Option<&IrFuncParam> {
        self.param_arena.get(param_id)
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
        self.inst_arena.attach_before(before, cur_inst);
        let bb = self.get_inst(cur_inst).unwrap().bb;
        self.get_inst_mut(before).unwrap().bb = bb;

        let bb = self.get_bb_mut(bb).unwrap();
        if bb.insts_head == Some(cur_inst) {
            bb.insts_head = Some(before);
        }
    }

    pub fn set_inst_after_cur(&mut self, after: InstId, cur_inst: InstId) {
        self.inst_arena.attach_after(after, cur_inst);
        let bb = self.get_inst(cur_inst).unwrap().bb;
        self.get_inst_mut(after).unwrap().bb = bb;

        let bb = self.get_bb_mut(bb).unwrap();
        if bb.insts_tail == Some(cur_inst) {
            bb.insts_tail = Some(after);
        }
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
        let new_inst_id = self.new_inst(inst_kind, ty, bb);
        let bb = self.get_bb_mut(bb).unwrap();
        let old_tail = bb.insts_tail.replace(new_inst_id);
        if bb.insts_head.is_none() {
            bb.insts_head = Some(new_inst_id);
        }
        if let Some(old_tail_id) = old_tail {
            let old_prev = self.get_inst(old_tail_id).unwrap().prev;
            let old_next = self.get_inst(old_tail_id).unwrap().next;
            let new_inst = self.get_inst_mut(new_inst_id).unwrap();
            new_inst.prev = old_prev;
            new_inst.next = old_next;
            self.set_inst_before_cur(old_tail_id, new_inst_id);
        }
        new_inst_id
    }

    pub fn build_inst_at_start(&mut self, inst_kind: InstKind, ty: IrTy, bb: BBId) -> InstId {
        let new_inst_id = self.new_inst(inst_kind, ty, bb);
        let bb = self.get_bb_mut(bb).unwrap();
        let old_head = bb.insts_head.replace(new_inst_id);
        if bb.insts_tail.is_none() {
            bb.insts_tail = Some(new_inst_id);
        }
        if let Some(old_head_id) = old_head {
            let old_prev = self.get_inst(old_head_id).unwrap().prev;
            let old_next = self.get_inst(old_head_id).unwrap().next;
            let new_inst = self.get_inst_mut(new_inst_id).unwrap();
            new_inst.prev = old_prev;
            new_inst.next = old_next;
            self.set_inst_after_cur(old_head_id, new_inst_id);
        }
        new_inst_id
    }
}

impl IrFunc {
    pub fn get_bb(&self, bb_id: BBId) -> Option<&BasicBlock> {
        self.bb_arena.get(bb_id)
    }

    pub fn get_bb_mut(&mut self, bb_id: BBId) -> Option<&mut BasicBlock> {
        self.bb_arena.get_mut(bb_id)
    }

    pub fn new_bb(&mut self) -> BBId {
        self.bb_arena.insert(BasicBlock::default())
    }

    pub fn set_bb_before_cur(&mut self, before: BBId, cur_bb: BBId) {
        self.bb_arena.attach_before(before, cur_bb);
    }

    pub fn set_bb_after_cur(&mut self, after: BBId, cur_bb: BBId) {
        self.bb_arena.attach_after(after, cur_bb);
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

    pub fn build_bb(&mut self) -> BBId {
        let new_bb = self.new_bb();
        if self.first_block.is_none() {
            self.first_block = Some(new_bb);
        }
        new_bb
    }
}