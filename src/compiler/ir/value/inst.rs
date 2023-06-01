use enum_as_inner::EnumAsInner;

use crate::compiler::intrusive_linkedlist::IntrusiveLinkedListItem;
use crate::compiler::syntax::ast::BinaryOp;

use super::{
    super::arena::{BBId, FuncId, InstId},
    ty::IrTy,
    value::{Operand, Value},
};

#[derive(Debug, Clone)]
pub struct Inst {
    pub kind: InstKind,
    pub ty: IrTy,

    pub bb: BBId,
    pub prev: Option<InstId>,
    pub next: Option<InstId>,
}

impl Value for Inst {
    fn get_ty(&self) -> &IrTy {
        &self.ty
    }
}

impl IntrusiveLinkedListItem for Inst {
    type Key = InstId;

    fn next(&self) -> Option<Self::Key> {
        self.next
    }

    fn set_next(&mut self, key: Option<Self::Key>) {
        self.next = key;
    }

    fn prev(&self) -> Option<Self::Key> {
        self.prev
    }

    fn set_prev(&mut self, key: Option<Self::Key>) {
        self.prev = key;
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InstKind {
    // Binary Instruction
    Binary(Binary),

    // Terminator Instruction
    Br(Br),
    RetInst(RetInst),

    // Memory
    Alloca(Alloca),
    Load(Load),
    Store(Store),
    GEP(GEP),

    // Conversion
    ZExt(ZExt),

    // Other
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: BinaryInstOp,
    pub left: Operand,
    pub right: Operand,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryInstOp {
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
    #[must_use] pub fn to_binary_inst_kind(&self) -> BinaryInstOp {
        match self {
            BinaryOp::Add => BinaryInstOp::Add,
            BinaryOp::Sub => BinaryInstOp::Sub,
            BinaryOp::Mul => BinaryInstOp::Mul,
            BinaryOp::Div => BinaryInstOp::Div,
            BinaryOp::Mod => BinaryInstOp::Mod,
            BinaryOp::Gt => BinaryInstOp::Gt,
            BinaryOp::Lt => BinaryInstOp::Lt,
            BinaryOp::Ge => BinaryInstOp::Ge,
            BinaryOp::Le => BinaryInstOp::Le,
            BinaryOp::Eq => BinaryInstOp::Eq,
            BinaryOp::Ne => BinaryInstOp::Ne,
            BinaryOp::And => BinaryInstOp::And,
            BinaryOp::Or => BinaryInstOp::Or,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Br {
    Br { cond: Operand, true_bb: BBId, false_bb: BBId },
    Jump { nxt_bb: BBId },
}

#[derive(Debug, Clone)]
pub struct RetInst {
    pub val: Option<Operand>,
}

#[derive(Debug, Clone)]
pub struct Alloca {
    pub alloca_ty: IrTy,
}

#[derive(Debug, Clone)]
pub struct Load {
    pub addr: Operand,
}

#[derive(Debug, Clone)]
pub struct Store {
    pub addr: Operand,
    pub data: Operand,
}

#[derive(Debug, Clone)]
pub struct GEP {
    pub ptr: Operand,
    pub indices: Vec<Operand>,
}

#[derive(Debug, Clone)]
pub struct ZExt {
    pub ori_val: Operand,
    pub target_ty: IrTy,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub func_id: FuncId,
    pub args: Vec<Operand>,
}
