use std::fmt::{Display, Formatter};
use slotmap::SlotMap;

use crate::compiler::ir::arena::{FuncId, GlobalId};

use super::{func::IrFunc, global::GlobalVar};

#[derive(Debug)]
pub struct Module {
    global_arena: SlotMap<GlobalId, GlobalVar>,
    func_arena: SlotMap<FuncId, IrFunc>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            global_arena: SlotMap::with_key(),
            func_arena: SlotMap::with_key(),
        }
    }

    pub fn build_func(&mut self, func: IrFunc) -> FuncId {
        self.func_arena.insert(func)
    }

    pub fn get_func(&self, func_id: FuncId) -> Option<&IrFunc> {
        self.func_arena.get(func_id)
    }

    pub fn get_func_mut(&mut self, func_id: FuncId) -> Option<&mut IrFunc> {
        self.func_arena.get_mut(func_id)
    }

    pub fn get_global(&self, global_id: GlobalId) -> Option<&GlobalVar> {
        self.global_arena.get(global_id)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}