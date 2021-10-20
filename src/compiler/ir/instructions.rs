use std::sync::Weak;
use enum_as_inner::EnumAsInner;

use crate::compiler::ir::values::*;
use crate::compiler::ptr::{MutRc, MutWeak};
use crate::compiler::syntax::ast::BinaryOp;

#[derive(Debug, Clone)]
pub struct Inst {
    pub kind: InstKind,
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
    pub left: Use,
    pub right: Use,
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

impl BinaryInst {
    pub fn from(op: BinaryInstKind, lhs: MutWeak<Value>, rhs: MutWeak<Value>) -> BinaryInst {
        todo!()
    }
}

impl BinaryOp {
    pub fn to_binary_inst_kind(&self) -> BinaryInstKind {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum BranchInst {
    Br { cond: Use, true_blk: Use, false_blk: Use },
    Jump { nxt_blk: Use },
}

#[derive(Debug, Clone)]
pub struct RetInst {
    pub ret_val: Use,
}

#[derive(Debug, Clone)]
pub struct LoadInst {
    pub addr: Use,
}

#[derive(Debug, Clone)]
pub struct StoreInst {
    pub addr: Use,
}

#[derive(Debug, Clone)]
pub struct GEPInst {
    ptr: Use,
    indices: Vec<Use>,
}

#[derive(Debug, Clone)]
pub struct CallInst {
    func: Use,
    args: Vec<Use>,
}

impl CallInst {
    pub fn from(func: MutWeak<Value>, args: Vec<MutWeak<Value>>) -> MutRc<CallInst> {
        todo!()
    }
}