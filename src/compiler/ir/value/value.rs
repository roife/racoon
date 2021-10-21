use std::collections::HashMap;
use super::func::Func;

// #[derive(Debug, Clone)]
// pub struct Use {
//     pub value: MutWeak<Value>,
//     pub user: MutWeak<Value>,
// }

// #[derive(Debug, Clone)]
// pub struct Value {
//     pub ty: Ty,
//     pub uses: Option<LinkedList<Use>>,
//     pub value_kind: ValueKind,
// }
//
// #[derive(Debug, Clone)]
// pub enum ValueKind {
//     Module(Module),
//     Func(Func),
//     BasicBlock(BasicBlock),
//     Constant(Constant),
//     Global(GlobalVar),
//     Inst(Inst),
// }

#[derive(Debug, Clone)]
pub struct Module {
    pub global_vars: HashMap<String, GlobalVar>,
    pub funcs: HashMap<String, Func>,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
}
