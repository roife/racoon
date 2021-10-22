use slotmap::SlotMap;

use crate::compiler::ir::arena::{BBId, FuncId, GlobalId, InstId};

use super::{basic_block::BasicBlock, func::Func, global::GlobalVar, inst::Inst};

#[derive(Debug, Clone)]
pub struct Module {
    global_arena: SlotMap<GlobalId, GlobalVar>,
    func_arena: SlotMap<FuncId, Func>,
}

impl Module {
    pub fn get_func(&self, func_id: FuncId) -> Option<&Func> {
        self.func_arena.get(func_id)
    }

    pub fn get_func_mut(&mut self, func_id: FuncId) -> Option<&mut Func> {
        self.func_arena.get_mut(func_id)
    }

    pub fn get_global(&self, global_id: GlobalId) -> Option<&GlobalVar> {
        self.global_arena.get(global_id)
    }
}