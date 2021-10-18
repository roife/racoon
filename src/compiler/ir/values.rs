use std::{
    cell::RefCell,
    collections::LinkedList,
    rc::{Rc, Weak},
};

use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone)]
pub enum Ty {
    Void,
    Func {
        ret_ty: Box<Ty>,
        params: Vec<Box<Ty>>,
    },
    Int(usize),
    Ptr(Box<Ty>),
    Label,
    Array(Box<Ty>),
}

#[derive(Debug, Clone)]
pub struct Use {
    pub value: Weak<RefCell<Value>>,
    pub user: Weak<RefCell<Value>>,
}

#[derive(Debug, Clone)]
pub struct Value {
    pub ty: Ty,
    pub uses: Option<LinkedList<Use>>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub value: Value,
    // funcs
    // decls
}

#[derive(Debug, Clone)]
pub struct Func {
    pub value: Value,
    // bbs
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub value: Value,
    pub succ_blks: Vec<Weak<RefCell<BasicBlock>>>,
    // inst
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
    Array(Vec<Box<Constant>>),
}

#[derive(Debug, Clone)]
pub struct Inst {
    pub value: Value,
    pub kind: InstKind,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum InstKind {
    // Binary Instruction
    Binary {
        kind: BinaryInstKind,
        left: Use,
        right: Use,
    },

    // Terminator Instruction
    Branch {
        cond: Use,
        true_blk: Use,
        false_blk: Use,
    },
    Jump { nxt_blk: Use },
    ReturnInst { ret_val: Use },

    // Memory
    Alloca,
    Load { addr: Use },
    Store { addr: Use },
    GEP {
        ptr: Use,
        indices: Vec<Use>,
    },

    // Conversion
    // ZExt(ZExtInst),

    // Other
    Call {
        func: Use,
        args: Vec<Use>,
    },
    Phi {
        incoming_vals: Vec<Use>,
        incoming_blks: Vec<Use>,
    },
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
