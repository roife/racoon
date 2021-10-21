use slotmap::SlotMap;

use crate::compiler::ir::arena::{BBId, InstId};
use crate::compiler::ir::value::{basic_block::BasicBlock, inst::Inst};

use super::{ty::Ty, value::Value};

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub ret_ty: Ty,
    pub is_builtin: bool,
    // pub params:
    pub first_block: Option<BBId>,
}

impl Value for Func {
    fn get_ty(&self) -> Ty {
        todo!()
    }
}

impl Func {
    pub fn new(name: &str, ret_ty: Ty, is_builtin: bool) -> Func {
        Func {
            name: String::from(name),
            ret_ty,
            is_builtin,
            first_block: None,
        }
    }
}
