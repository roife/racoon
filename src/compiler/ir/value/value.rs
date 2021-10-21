use slotmap::SlotMap;

use crate::compiler::ir::arena::{ArenaItem, BBId, FuncId, GVId, InstId};

use super::{
    func::FuncItem,
    ty::Ty,
};

#[derive(Debug, Clone)]
pub struct Value {
    pub ty: Ty,
    // pub uses: todo
    pub value_kind: ValueKind,
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    GlobalVar(GVId),
    Func(FuncId),
    BasicBlock(BBId),
    Inst(InstId),
    Constant(Constant),
}

#[derive(Debug, Clone)]
pub struct Module {
    global_var_arena: SlotMap<GVId, GVItem>,
    func_arena: SlotMap<FuncId, FuncItem>,
}

pub type GVItem = ArenaItem<GVId, GlobalVar>;

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
}
