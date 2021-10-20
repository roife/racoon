use std::rc::Weak;
use crate::compiler::ir::values::BasicBlock;
use crate::compiler::ptr::{MutRc, MutWeak};

impl BasicBlock {
    pub fn new() -> BasicBlock {
        BasicBlock {
            succ_blks: vec![],
            insts: Default::default()
        }
    }

    pub fn set_nxt_blk(&mut self, nxt_blk: MutWeak<BasicBlock>) {
        todo!()
    }
}