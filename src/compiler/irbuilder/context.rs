use std::collections::{
    hash_map::Entry,
    HashMap,
};
use crate::compiler::ir::arena::{BBId, FuncId};

pub struct Var;

pub struct Scope {
    vars: HashMap<String, Var>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { vars: HashMap::new() }
    }

    pub fn find(&self, name: &str) -> Option<&Var> {
        self.vars.get(name)
    }

    pub fn insert(&mut self, name: String, value: Var) -> Option<&Var> {
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

    pub fn find_name_rec(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.find(name) {
                return Some(value)
            }
        }
        None
    }

    pub fn insert(&mut self, name: &str) -> Option<&Var> {
        let name = String::from(name);
        let var: Var = Var;
        self.scopes.last_mut().expect("No scope found")
            .insert(name, var)
    }
}

pub struct Context {
    scope_builder: ScopeBuilder,
    cur_func: FuncId,
    cur_bb: BBId,
}