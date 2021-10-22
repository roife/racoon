use std::collections::{
    hash_map::Entry,
    HashMap,
};

use enum_as_inner::EnumAsInner;

use crate::compiler::ir::{
    arena::{BBId, FuncId, GlobalId, InstId},
    value::ty::Ty,
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

struct BreakTarget {
    pub break_out: BBId,
    pub continue_in: BBId,
}

pub struct Context<'a> {
    pub scope_builder: ScopeBuilder<NameId>,
    break_targets: Vec<BreakTarget>,
    cur_module: &'a mut Module,
    cur_func: FuncId,
    cur_bb: BBId,
}

impl<'a> Context<'a> {
    fn get_cur_func_mut(&mut self) -> &mut Func {
        self.cur_module.get_func_mut(self.cur_func).unwrap()
    }

    fn get_cur_bb_mut(&mut self) -> &mut BasicBlock {
        let bb = self.cur_bb;
        self.get_cur_func_mut().get_bb_mut(bb).unwrap()
    }

    pub fn set_cur_bb(&mut self, bb: BBId) {
        self.cur_bb = bb;
    }

    pub fn build_inst_at_end(&mut self, inst_kind: InstKind, ty: Ty) -> InstId {
        let bb = self.cur_bb;
        self.get_cur_func_mut().build_inst_at_end(inst_kind, ty, bb)
    }

    pub fn build_bb_after_cur(&mut self) -> BBId {
        let bb = self.cur_bb;
        self.get_cur_func_mut().build_bb_after_cur(bb)
    }
}
