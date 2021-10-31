use std::fmt::{Display, Formatter};
use slotmap::SlotMap;
use crate::compiler::intrusive_linkedlist::IntrusiveLinkedList;

use crate::compiler::ir::arena::{FuncId, GlobalId};

use super::{func::IrFunc, global::Global};

#[derive(Debug)]
pub struct Module {
    first_func: Option<FuncId>,
    first_global: Option<GlobalId>,

    global_arena: SlotMap<GlobalId, Global>,
    func_arena: SlotMap<FuncId, IrFunc>,
}

impl Module {
    pub fn new() -> Module {
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

    pub fn get_func(&self, func_id: FuncId) -> Option<&IrFunc> {
        self.func_arena.get(func_id)
    }

    pub fn get_func_mut(&mut self, func_id: FuncId) -> Option<&mut IrFunc> {
        self.func_arena.get_mut(func_id)
    }

    pub fn get_global(&self, global_id: GlobalId) -> Option<&Global> {
        self.global_arena.get(global_id)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.global_arena
            .items_iter(self.first_global, None)
            .try_for_each(|(_, global)|
                writeln!(f, "@{} = global {}", global.name, global.init_val)
            )?;

        self.func_arena
            .items_iter(self.first_func, None)
            .for_each(|(_, func)| {
                let param_str = func.params.iter()
                    .map(|x| format!("{}", func.get_param(*x).unwrap()))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "define {}({}){}", func, param_str, func.first_block.unwrap());
            });
        Ok(())
    }
}