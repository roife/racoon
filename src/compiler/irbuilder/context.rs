use std::collections::{
    hash_map::Entry,
    HashMap,
};

use enum_as_inner::EnumAsInner;

use crate::compiler::ir::{
    arena::{BBId, FuncId, GlobalId, InstId},
    value::ty::IrTy,
};
use crate::compiler::ir::value::basic_block::BasicBlock;
use crate::compiler::ir::value::func::Func;
use crate::compiler::ir::value::inst::InstKind;
use crate::compiler::ir::value::module::Module;

#[derive(Debug, Clone, EnumAsInner)]
pub enum NameId {
    Inst(InstId),
    Func(FuncId),
    Global(GlobalId),
}

pub struct Scope<T> {
    vars: HashMap<String, T>,
}

impl<T> Scope<T> {
    pub fn new() -> Scope<T> {
        Scope { vars: HashMap::new() }
    }

    pub fn find(&self, name: &str) -> Option<&T> {
        self.vars.get(name)
    }

    pub fn insert(&mut self, name: String, value: T) -> Option<&T> {
        let entry = self.vars.entry(name);
        match entry {
            Entry::Occupied(_) => None,
            Entry::Vacant(e) => Some(e.insert(value))
        }
    }
}

pub struct ScopeBuilder<T> {
    scopes: Vec<Scope<T>>,
}

impl<T> ScopeBuilder<T> {
    pub fn new() -> ScopeBuilder<T> {
        ScopeBuilder {
            scopes: vec![]
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn find_name_rec(&self, name: &str) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.find(name) {
                return Some(value)
            }
        }
        None
    }

    pub fn insert(&mut self, name: &str, id: T) -> Option<&T> {
        let name = String::from(name);
        self.scopes.last_mut().expect("No scope found")
            .insert(name, id)
    }
}

struct BCTarget {
    pub break_target: BBId,
    pub continue_target: BBId,
}

pub struct IrBuilderContext<'a> {
    pub scope_builder: ScopeBuilder<NameId>,
    cur_module: &'a mut Module,
    cur_func: FuncId,
    cur_bb: BBId,

    bc_targets: Vec<BCTarget>,
}

impl<'a> IrBuilderContext<'a> {
    pub fn get_cur_bb_id(&self) -> BBId {
        self.cur_bb
    }

    fn get_cur_func_mut(&mut self) -> &mut Func {
        self.cur_module.get_func_mut(self.cur_func).unwrap()
    }

    fn get_cur_bb_mut(&mut self) -> &mut BasicBlock {
        let bb = self.cur_bb;
        self.get_cur_func_mut().get_bb_mut(bb).unwrap()
    }

    pub fn get_func_ret_ty(&self) -> IrTy {
        self.cur_module.get_func(self.cur_func).unwrap().ret_ty.clone()
    }

    pub fn set_cur_bb(&mut self, bb: BBId) {
        self.cur_bb = bb;
    }

    pub fn build_inst_at_end_of(&mut self, inst_kind: InstKind, ty: IrTy, bb: BBId) -> InstId {
        self.get_cur_func_mut().build_inst_at_end(inst_kind, ty, bb)
    }

    pub fn build_inst_at_end_of_cur(&mut self, inst_kind: InstKind, ty: IrTy) -> InstId {
        let cur_bb = self.cur_bb;
        self.build_inst_at_end_of(inst_kind, ty, cur_bb)
    }

    pub fn build_bb(&mut self) -> BBId {
        let bb = self.cur_bb;
        self.get_cur_func_mut().build_bb_after_cur(bb)
    }
}

impl<'a> IrBuilderContext<'a> {
    pub fn push_break_target(&mut self, break_target: BBId, continue_target: BBId) {
        self.bc_targets.push(BCTarget {
            break_target,
            continue_target
        });
    }

    pub fn pop_break_target(&mut self) {
        self.bc_targets.pop();
    }

    pub fn get_break_target(&self) -> Option<BBId> {
        Some(self.bc_targets.last()?.break_target)
    }

    pub fn get_continue_target(&self) -> Option<BBId> {
        Some(self.bc_targets.last()?.continue_target)
    }
}