use slotmap::SlotMap;

use crate::compiler::ir::arena::{BBId, FuncId, GlobalId, InstId};

use super::{basic_block::BasicBlock, func::Func, global::GlobalVar, inst::Inst};

#[derive(Debug, Clone)]
pub struct Module {
    global_arena: SlotMap<GlobalId, GlobalVar>,
    func_arena: SlotMap<FuncId, Func>,
    inst_arena: SlotMap<InstId, Inst>,
    bb_arena: SlotMap<BBId, BasicBlock>,
}

impl Module {
    pub fn get_func(&self, func_id: FuncId) -> Option<&Func> {
        self.func_arena.get(func_id)
    }

    pub fn get_inst(&self, inst_id: InstId) -> Option<&Inst> {
        self.inst_arena.get(inst_id)
    }

    pub fn get_bb(&self, bb_id: BBId) -> Option<&BasicBlock> {
        self.bb_arena.get(bb_id)
    }

    pub fn get_global(&self, global_id: GlobalId) -> Option<&GlobalVar> {
        self.global_arena.get(global_id)
    }
}