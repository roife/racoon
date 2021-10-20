use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Weak,
};
use crate::compiler::ptr::MutWeak;

use super::{
    err::Error,
    super::ir::values::Value,
};

pub struct Scope {
    vars: HashMap<String, MutWeak<Value>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: HashMap::new()
        }
    }

    pub fn find(&self, name: &str) -> Option<MutWeak<Value>> {
        self.vars.get(name).map(|val| val.clone())
    }

    pub fn check_and_push_name(&mut self, name: &str, value: MutWeak<Value>) -> Result<(), Error> {
        if self.find(name).is_none() {
            self.vars.insert(String::from(name), value);
            Ok(())
        } else {
            Err(Error::DuplicateName(String::from(name)))
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

    pub fn find_name_rec(&self, name: &str) -> Result<MutWeak<Value>, Error> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.find(name) {
                return Ok(value)
            }
        }
        Err(Error::UnknownName(String::from(name)))
    }

    pub fn push_name(&mut self, name: &str, value: MutWeak<Value>) -> Result<(), Error> {
        match self.scopes.last_mut() {
            None => unreachable!(),
            Some(scope) => scope.check_and_push_name(name, value),
        }
    }
}
