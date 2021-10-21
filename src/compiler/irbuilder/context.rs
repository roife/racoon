use std::collections::{
    hash_map::Entry,
    HashMap,
};

use enum_as_inner::EnumAsInner;

use crate::compiler::ir::{
    arena::{BBId, FuncId, GlobalId, InstId},
    value::ty::Ty,
};

pub struct Name {
    pub is_global: bool,
    pub ty: Ty,
    pub id: NameId,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum NameId {
    Inst(InstId),
    Func(FuncId),
    Global(GlobalId),
}

pub struct Scope {
    vars: HashMap<String, Name>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { vars: HashMap::new() }
    }

    pub fn find(&self, name: &str) -> Option<&Name> {
        self.vars.get(name)
    }

    pub fn insert(&mut self, name: String, value: Name) -> Option<&Name> {
        let entry = self.vars.entry(name);
        match entry {
            Entry::Occupied(_) => None,
            Entry::Vacant(e) => Some(e.insert(value))
        }
    }
}

pub struct ScopeBuilder {
    scopes: Vec<Scope>,
}

impl ScopeBuilder {
    pub fn new() -> ScopeBuilder {
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

    pub fn find_name_rec(&self, name: &str) -> Option<&Name> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.find(name) {
                return Some(value)
            }
        }
        None
    }

    pub fn insert(&mut self, name: &str, id: NameId, ty: Ty, is_global: bool) -> Option<&Name> {
        let name = String::from(name);
        let var = Name {
            is_global,
            ty,
            id
        };
        self.scopes.last_mut().expect("No scope found")
            .insert(name, var)
    }
}

pub struct Context {
    pub scope_builder: ScopeBuilder,
    pub cur_func: FuncId,
    pub cur_bb: BBId,
}
