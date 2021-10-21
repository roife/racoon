use slotmap::SlotMap;
use crate::compiler::ir::arena::{GlobalId, FuncId};
use super::{func::Func, global::GlobalVar};

#[derive(Debug, Clone)]
pub struct Module {
    global_var_arena: SlotMap<GlobalId, GlobalVar>,
    func_arena: SlotMap<FuncId, Func>,
}

impl Module {
    pub fn get_func(&self, func_id: FuncId) -> Option<&Func> {
        self.func_arena.get(func_id)
    }
}