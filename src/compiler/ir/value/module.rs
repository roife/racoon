use slotmap::SlotMap;

use crate::compiler::ir::{
    arena::{FuncId, GlobalId},
};

use super::{func::IrFunc, global::Global};

#[derive(Debug)]
pub struct Module {
    pub first_func: Option<FuncId>,
    pub first_global: Option<GlobalId>,

    pub global_arena: SlotMap<GlobalId, Global>,
    pub func_arena: SlotMap<FuncId, IrFunc>,
}

impl Module {
    #[must_use] pub fn new() -> Module {
        Module {
            first_func: None,
            first_global: None,
            global_arena: SlotMap::with_key(),
            func_arena: SlotMap::with_key(),
        }
    }

    pub fn build_func(&mut self, func: IrFunc) -> FuncId {
        let id = self.func_arena.insert(func);
        let func = self.func_arena.get_mut(id).unwrap();
        func.next = self.first_func;
        self.first_func = Some(id);
        id
    }

    pub fn build_global(&mut self, global: Global) -> GlobalId {
        let id = self.global_arena.insert(global);
        let global = self.global_arena.get_mut(id).unwrap();
        global.next = self.first_global;
        self.first_global = Some(id);
        id
    }

    #[must_use] pub fn get_func(&self, func_id: FuncId) -> Option<&IrFunc> {
        self.func_arena.get(func_id)
    }

    pub fn get_func_mut(&mut self, func_id: FuncId) -> Option<&mut IrFunc> {
        self.func_arena.get_mut(func_id)
    }

    #[must_use] pub fn get_global(&self, global_id: GlobalId) -> Option<&Global> {
        self.global_arena.get(global_id)
    }
}
