use std::collections::{
    hash_map::Entry,
    HashMap,
};

use enum_as_inner::EnumAsInner;

use crate::compiler::ir::{
    arena::{BBId, FuncId, GlobalId, InstId},
    value::{basic_block::BasicBlock, func::IrFunc, inst::InstKind, module::Module, ty::IrTy},
};
use crate::compiler::ir::arena::ParamId;
use crate::compiler::ir::value::global::GlobalVar;
use crate::compiler::ir::value::ty::FuncTy;
use crate::compiler::ir::value::value::Value;
use crate::compiler::syntax::ast::AstTy;

#[derive(Debug, Clone, EnumAsInner)]
pub enum NameId {
    Inst(InstId),
    Func(FuncId),
    Global(GlobalId),
    Param(ParamId),
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

pub struct BCTarget {
    pub break_target: BBId,
    pub continue_target: BBId,
}

pub struct Context {
    pub scope_builder: ScopeBuilder<NameId>,
    pub cur_module: Module,
    cur_func: FuncId,
    cur_bb: BBId,
}

impl Context {
    pub fn new() -> Context {
        Context {
            scope_builder: ScopeBuilder::new(),
            cur_module: Module::new(),
            cur_func: Default::default(),
            cur_bb: Default::default(),
        }
    }

    pub fn get_cur_bb_id(&self) -> BBId {
        self.cur_bb
    }

    fn get_cur_func_mut(&mut self) -> &mut IrFunc {
        self.cur_module.get_func_mut(self.cur_func).unwrap()
    }

    fn get_cur_bb_mut(&mut self) -> &mut BasicBlock {
        let bb = self.cur_bb;
        self.get_cur_func_mut().get_bb_mut(bb).unwrap()
    }

    pub fn get_func_ty(&self, func: FuncId) -> FuncTy {
        let func_ty = self.cur_module.get_func(self.cur_func).unwrap().get_ty();
        *func_ty.into_func().unwrap()
    }

    pub fn set_cur_bb(&mut self, bb: BBId) {
        self.cur_bb = bb;
    }

    pub fn set_cur_func(&mut self, func: FuncId) {
        self.cur_func = func;
    }

    pub fn build_inst_end(&mut self, inst_kind: InstKind, ty: IrTy, bb: BBId) -> InstId {
        self.get_cur_func_mut().build_inst_at_end(inst_kind, ty, bb)
    }

    pub fn build_inst_end_of_cur(&mut self, inst_kind: InstKind, ty: IrTy) -> InstId {
        let cur_bb = self.cur_bb;
        self.build_inst_end(inst_kind, ty, cur_bb)
    }

    pub fn build_bb_after_cur(&mut self) -> BBId {
        let bb = self.cur_bb;
        self.get_cur_func_mut().build_bb_after_cur(bb)
    }

    pub fn build_bb(&mut self) -> BBId {
        self.get_cur_func_mut().build_bb()
    }

    pub fn build_func(&mut self, func: IrFunc) -> FuncId {
        self.cur_module.build_func(func)
    }

    pub fn build_global(&mut self, global: GlobalVar) -> GlobalId {
        self.cur_module.build_global(global)
    }

    pub fn build_func_param(&mut self, ty: IrTy) -> ParamId {
        self.get_cur_func_mut().build_func_param(ty)
    }
}

impl From<AstTy> for IrTy {
    fn from(ast_ty: AstTy) -> Self {
        match ast_ty {
            AstTy::Void => IrTy::Void,
            AstTy::Int => IrTy::Int(32),
            AstTy::Bool => IrTy::Int(1),
            AstTy::Unknown => unreachable!(),
            AstTy::Func { ret_ty, param_tys: params } => {
                IrTy::Func(Box::new(FuncTy {
                    ret_ty: ret_ty.as_ref().clone().into(),
                    params_ty: params.iter().map(|x| x.as_ref().clone().into()).collect(),
                }))
            }
        }
    }
}
