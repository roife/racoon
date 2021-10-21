use enum_as_inner::EnumAsInner;
use crate::compiler::syntax::ast::BinaryOp;
use super::{
    super::arena::{BBId, InstId},
    ty::Ty,
    basic_block::BasicBlock,
};

#[derive(Debug, Clone)]
pub struct Inst {
    pub kind: InstKind,
    pub ty: Ty,

    pub bb: BBId,
    pub prev: Option<InstId>,
    pub next: Option<InstId>,
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
    // pub kind: BinaryInstKind,
    // pub left: InstId,
    // pub right: InstId,
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
    // Br { cond: (), true_blk: BBId, false_blk: BBId },
    // Jump { nxt_blk: BBId },
}

#[derive(Debug, Clone)]
pub struct RetInst {
    // pub ret_val: InstId,
}

#[derive(Debug, Clone)]
pub struct LoadInst {
    // pub addr: Use,
}

#[derive(Debug, Clone)]
pub struct StoreInst {
    // pub addr: Use,
}

#[derive(Debug, Clone)]
pub struct GEPInst {
    // ptr: Use,
    // indices: Vec<Use>,
}

#[derive(Debug, Clone)]
pub struct CallInst {
    // func: Use,
    // args: Vec<Use>,
}
