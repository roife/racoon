use std::{
    cell::RefCell,
    collections::LinkedList,
    rc::{Rc, Weak},
};

use enum_as_inner::EnumAsInner;

use crate::compiler::ir::instructions::Inst;
use crate::compiler::ptr::*;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Ty {
    Void,
    Func(FuncTy),
    Int(usize),
    Ptr(Box<Ty>),
    Label,
    Array(Box<Ty>),
}

#[derive(Debug, Clone)]
pub struct FuncTy {
    pub ret_ty: Box<Ty>,
    pub params: Vec<Box<Ty>>
}

#[derive(Debug, Clone)]
pub struct Use {
    pub value: MutWeak<Value>,
    pub user: MutWeak<Value>,
}

impl Use {
    pub fn from(user: MutWeak<Value>, value: MutWeak<Value>) -> Use {
        Use { value, user }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub ty: Ty,
    pub uses: Option<LinkedList<Use>>,
    pub value_kind: ValueKind,
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    Module(Module),
    Func(Func),
    BasicBlock(BasicBlock),
    Constant(Constant),
    Inst(Inst),
}

#[derive(Debug, Clone)]
pub struct Module {
    // funcs
    // decls
}

#[derive(Debug, Clone)]
pub struct Func {
    pub is_builtin: bool,
    pub name: String,
    pub params: Vec<MutRc<Value>>,
    pub basic_blocks: LinkedList<MutRc<BasicBlock>>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub succ_blks: Vec<Weak<RefCell<BasicBlock>>>,
    pub insts: LinkedList<MutRc<Inst>>,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
    Array(Vec<Box<Constant>>),
}

#[derive(Debug, Clone)]
pub enum GlobalVariable {}
