use std::sync::{Arc, Mutex};

use crate::FunctionSignature;

#[derive(Debug)]
pub struct ScopeManager {
    scopes: Vec<Scope>,
    current_scope: usize,
}

#[derive(Debug)]
pub enum ScopeEntry {
    Function(Arc<Mutex<FunctionSignature>>),
    // Variable(Rc<VariableSignature>)
}
impl ScopeEntry {
    /// Returns the name of an entry.
    fn name(&self) -> &str {
        todo!()
    }
}

#[derive(Debug)]
pub struct Scope {
    _index: usize,
    _type: ScopeType,
    /// The index of the parent scope in the scope manager.
    parent_index: Option<usize>,
    /// The index of the children scopes in the scope manager.
    children_index: Vec<usize>,
    entries: Vec<ScopeEntry>,
}

#[derive(Debug)]
pub enum ScopeType {
    Global,
    Functional,
    Local,
}

impl Scope {
    /// Creates a global scope.
    pub fn global() -> Self {
        Scope {
            _index: 0,
            _type: ScopeType::Global,
            parent_index: None,
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Creates a child of another scope.
    fn local(_index: usize, parent: usize, _type: ScopeType) -> Self {
        Scope {
            _index,
            _type,
            parent_index: Some(parent),
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Find an item inside the current scope.
    fn find(&self, name: &str) -> Option<&ScopeEntry> {
        for entry in &self.entries {
            if entry.name() == name {
                return Some(entry);
            }
        }
        None
    }
    /// Add an entry to the scope.
    fn add(&mut self, entry: ScopeEntry) {
        self.entries.push(entry)
    }
}

impl Default for ScopeManager {
    fn default() -> Self {
        ScopeManager::new()
    }
}

impl ScopeManager {
    pub fn new() -> Self {
        ScopeManager {
            scopes: vec![Scope::global()],
            current_scope: 0,
        }
    }
    /// Checks if the program is currently in the global scope.
    pub fn is_global(&self) -> bool {
        self.current_scope == 0
    }
    /// Returns the number of current nested scopes.
    pub fn depth(&self) -> usize {
        let mut depth = 0;
        let mut index = self.current_scope;

        loop {
            match self.scopes[index].parent_index {
                Some(idx) => {
                    index = idx;
                    depth += 1;
                }
                None => break,
            }
        }
        depth
    }
    /// Creates a new local scope and enters it.
    pub fn enter(&mut self, _type: ScopeType) {
        let new_scope_index = self.scopes.len();
        self.scopes
            .push(Scope::local(new_scope_index, self.current_scope, _type));
        self.scopes[self.current_scope]
            .children_index
            .push(new_scope_index);
        self.current_scope = new_scope_index;
    }
    /// Search within the current scope for an entry.
    pub fn search_in_current(&self, name: &str) -> Option<&ScopeEntry> {
        let scope = &self.scopes[self.current_scope];
        scope.find(name)
    }
    /// Search for an entry within the current scope, or within any of its parents.
    pub fn lookup(&self, name: &str) -> Option<&ScopeEntry> {
        let mut scope = &self.scopes[self.current_scope];
        loop {
            match scope.find(name) {
                Some(entry) => return Some(entry),
                None => match scope.parent_index {
                    Some(parent_index) => scope = &self.scopes[parent_index],
                    None => return None,
                },
            }
        }
    }
    /// Register a function as being present within a scope.
    pub fn register_function(&mut self, signature: Arc<Mutex<FunctionSignature>>) {
        self.scopes[self.current_scope].add(ScopeEntry::Function(signature));
    }

    /// Leaves the current scope and return to its parent.
    /// # Panics
    /// Panics if the current scope is the global scope.
    pub fn leave(&mut self) {
        if self.current_scope == 0 {
            panic!("Cannot leave global scope.")
        }
        self.current_scope = self.scopes[self.current_scope].parent_index.unwrap();
    }
}

#[cfg(test)]
mod tests {
    use crate::scope::ScopeType;

    use super::ScopeManager;

    #[test]
    fn test_depth() {
        let mut scope_manager = ScopeManager::new();

        assert_eq!(scope_manager.depth(), 0);

        scope_manager.enter(ScopeType::Local);
        scope_manager.enter(ScopeType::Local);
        assert_eq!(scope_manager.depth(), 2);

        scope_manager.enter(ScopeType::Local);
        scope_manager.enter(ScopeType::Local);
        assert_eq!(scope_manager.depth(), 4);

        scope_manager.leave();
        assert_eq!(scope_manager.depth(), 3);

        scope_manager.leave();
        scope_manager.leave();
        assert_eq!(scope_manager.depth(), 1);
    }
}
