use crate::FunctionSignature;

/// A hierarchical data structure that stores info in related "depths".
/// It provides functions for creating and managing the lifecycle of nested scopes.
#[derive(Debug)]
pub struct ScopeManager {
    scopes: Vec<Scope>,
    current_scope: usize,
}

/// An entry to the symbol table of a scope.
#[derive(Debug)]
pub enum ScopeEntry {
    Function(FunctionSignature),
}

#[derive(Debug)]
pub enum ScopeType {
    Global,
    Functional,
    Local,
}

/// The scope address stores the address of a symbol in the scope manager.
#[derive(Debug, PartialEq)]
pub struct ScopeAddress {
    /// The entry in which it is declared.
    pub scope: usize,
    /// The entry number.
    pub entry_no: usize,
}

#[derive(Debug)]
pub struct Scope {
    _type: ScopeType,
    /// The index of the parent scope in the scope manager.
    parent_index: Option<usize>,
    /// The index of the children scopes in the scope manager.
    children_index: Vec<usize>,
    entries: Vec<ScopeEntry>,
}

/// The result of a search through the scope manager.
#[derive(Debug)]
pub struct ScopeSearch<'a> {
    pub entry: &'a ScopeEntry,
    pub scope: &'a Scope,
}

impl From<[usize; 2]> for ScopeAddress {
    fn from(value: [usize; 2]) -> Self {
        ScopeAddress {
            scope: value[0],
            entry_no: value[1],
        }
    }
}

impl ScopeEntry {
    /// Returns the name of an entry.
    fn name(&self) -> &str {
        match self {
            ScopeEntry::Function(function) => &function.name.name,
        }
    }
}

impl Scope {
    /// Creates a global scope.
    pub fn global() -> Self {
        Scope {
            _type: ScopeType::Global,
            parent_index: None,
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Creates a child of another scope.
    fn local(_index: usize, parent: usize, _type: ScopeType) -> Self {
        Scope {
            _type,
            parent_index: Some(parent),
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Find an item inside the current scope.
    pub fn find(&self, name: &str) -> Option<&ScopeEntry> {
        for entry in &self.entries {
            if entry.name() == name {
                return Some(entry);
            }
        }
        None
    }
    /// Add an entry to the scope and returns its index.
    pub fn add(&mut self, entry: ScopeEntry) -> usize {
        self.entries.push(entry);
        self.entries.len() - 1
    }
    /// Get a function entry by its index.
    pub fn get_function(&self, entry_no: usize) -> Option<&FunctionSignature> {
        self.entries.get(entry_no).map(|entry| match entry {
            ScopeEntry::Function(f) => f,
        })
    }
}

impl Default for ScopeManager {
    fn default() -> Self {
        ScopeManager::new()
    }
}

impl ScopeManager {
    /// Create a new scope manager.
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
    /// Returns the number of scopes.
    pub fn len(&self) -> usize {
        self.scopes.len()
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
    /// Returns the index of the current scope.
    pub fn current(&self) -> usize {
        self.current_scope
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
    pub fn lookaround(&self, name: &str) -> Option<ScopeSearch> {
        let scope = &self.scopes[self.current_scope];
        scope.find(name).map(|entry| ScopeSearch { entry, scope })
    }
    /// Search for an entry within the current scope, or within any of its ancestors.
    pub fn lookup(&self, name: &str) -> Option<ScopeSearch> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            match scope.find(name) {
                Some(entry) => return Some(ScopeSearch { entry, scope }),
                None => match scope.parent_index {
                    Some(parent_index) => current_scope = parent_index,
                    None => return None,
                },
            }
        }
    }
    /// Search for an entry within the current scope, or within any of its descendants.
    pub fn lookdown(&self, name: &str) -> Option<ScopeSearch> {
        self.recursive_search(self.current_scope, name)
    }
    /// Lookdown helper.
    fn recursive_search(&self, current_scope: usize, name: &str) -> Option<ScopeSearch> {
        let scope = &self.scopes[current_scope];
        match scope.find(name) {
            Some(entry) => return Some(ScopeSearch { entry, scope }),
            None => {
                for child_idx in &scope.children_index {
                    let search = self.recursive_search(*child_idx, name);
                    if search.is_some() {
                        return search;
                    }
                }
            }
        };
        None
    }
    /// Register a function as being present within a scope.
    pub fn register_function(&mut self, signature: FunctionSignature) -> usize {
        self.scopes[self.current_scope].add(ScopeEntry::Function(signature))
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
    /// Returns the scope with the given index.
    pub fn get_scope(&self, index: usize) -> Option<&Scope> {
        self.scopes.get(index)
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
