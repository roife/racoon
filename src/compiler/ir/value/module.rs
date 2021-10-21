use slotmap::SlotMap;
use crate::compiler::ir::arena::{GlobalId, FuncId};
use super::{func::Func, global::GlobalVar};

#[derive(Debug, Clone)]
pub struct Module {
    global_var_arena: SlotMap<GlobalId, GlobalVar>,
    func_arena: SlotMap<FuncId, Func>,
}
