use enum_as_inner::EnumAsInner;

use crate::compiler::syntax::ast::BinaryOp;

use super::{
    super::arena::ArenaItem,
    value::Value,
};
use super::{
    super::arena::{BBId, InstId},
    ty::Ty,
};

pub type InstItem = ArenaItem<InstId, InstInfo>;

#[derive(Debug, Clone)]
pub struct InstInfo {
    pub kind: InstKind,
    pub ty: Ty,
    pub bb: BBId,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InstKind {
    // Binary Instruction
    Binary(BinaryInst),

    // Terminator Instruction
    Branch(BranchInst),
    ReturnInst(RetInst),

    // Memory
    Alloca,
    Load(LoadInst),
    Store(StoreInst),
    GEP(GEPInst),

    // Conversion
    // ZExt(ZExtInst),

    // Other
    Call(CallInst),
}

#[derive(Debug, Clone)]
pub struct BinaryInst {
    pub kind: BinaryInstKind,
    pub left: Value,
    pub right: Value,
}

#[derive(Debug, Clone)]
pub enum BinaryInstKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

impl BinaryOp {
    pub fn to_binary_inst_kind(&self) -> BinaryInstKind {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum BranchInst {
    Br { cond: Value, true_blk: Value, false_blk: Value },
    Jump { nxt_blk: Value },
}

#[derive(Debug, Clone)]
pub struct RetInst {
    pub ret_val: Value,
}

#[derive(Debug, Clone)]
pub struct LoadInst {
    // pub addr: Use,
}

#[derive(Debug, Clone)]
pub struct StoreInst {
    pub addr: Value,
}

#[derive(Debug, Clone)]
pub struct GEPInst {
    ptr: Value,
    indices: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct CallInst {
    func: Value,
    args: Vec<Value>,
}
