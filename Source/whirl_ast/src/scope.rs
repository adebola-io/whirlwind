use crate::{ClassSignature, EnumSignature, FunctionSignature, TypeSignature, VariableSignature};

/// A hierarchical data structure that stores info in related "depths".
/// It provides functions for creating and managing the lifecycle of nested scopes.
#[derive(Debug)]
pub struct ScopeManager {
    scopes: Vec<Scope>,
    current_scope: usize,
}

/// A shallow copy of the scope manager with a different current scope.
/// Allows for lookup without leaving the current scope.
pub struct ScopeManagerShadow<'a> {
    base: &'a ScopeManager,
    current_scope: usize,
}

/// An entry to the symbol table of a scope.
#[derive(Debug)]
pub enum ScopeEntry {
    Function(FunctionSignature),
    Type(TypeSignature),
    Class(ClassSignature),
    Enum(EnumSignature),
    Variable(VariableSignature),
}

#[derive(Debug)]
pub enum ScopeType {
    Global,
    Functional,
    Local,
    Test,
}

/// The scope address stores the address of a symbol in the scope manager.
#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct ScopeAddress {
    /// The entry in which it is declared.
    pub scope_id: usize,
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
    pub index: usize,
    pub entry: &'a ScopeEntry,
    pub scope: &'a Scope,
}

impl From<[usize; 2]> for ScopeAddress {
    fn from(value: [usize; 2]) -> Self {
        ScopeAddress {
            scope_id: value[0],
            entry_no: value[1],
        }
    }
}

impl ScopeEntry {
    /// Returns the name of an entry.
    pub fn name(&self) -> &str {
        match self {
            ScopeEntry::Function(FunctionSignature { name, .. })
            | ScopeEntry::Type(TypeSignature { name, .. })
            | ScopeEntry::Class(ClassSignature { name, .. })
            | ScopeEntry::Enum(EnumSignature { name, .. })
            | ScopeEntry::Variable(VariableSignature { name, .. }) => &name.name,
        }
    }

    /// Returns an entry as a variable signature. Panics if the entry is not a variable variant.
    pub fn var(&self) -> &VariableSignature {
        match self {
            ScopeEntry::Variable(v) => v,
            _ => panic!("{} is not a variable!", self.name()),
        }
    }

    /// Returns an entry as a mutable variable signature. Panics if the entry is not a variable variant.
    pub fn var_mut(&mut self) -> &mut VariableSignature {
        match self {
            ScopeEntry::Variable(v) => v,
            _ => panic!("{} is not a variable!", self.name()),
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
    pub fn find(&self, name: &str) -> Option<(usize, &ScopeEntry)> {
        for (index, entry) in self.entries.iter().enumerate() {
            if entry.name() == name {
                return Some((index, entry));
            }
        }
        None
    }
    /// Add an entry to the scope and returns its index.
    pub fn add(&mut self, entry: ScopeEntry) -> usize {
        self.entries.push(entry);
        self.entries.len() - 1
    }
    /// Get a class entry by its index.
    pub fn get_class(&self, entry_no: usize) -> Option<&ClassSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Class(c) => Some(c),
                _ => None,
            })
            .flatten()
    }
    /// Get a function entry by its index.
    pub fn get_function(&self, entry_no: usize) -> Option<&FunctionSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Function(f) => Some(f),
                _ => None,
            })
            .flatten()
    }
    /// Get a type entry by its index.
    pub fn get_type(&self, entry_no: usize) -> Option<&TypeSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Type(t) => Some(t),
                _ => None,
            })
            .flatten()
    }
    /// Get a enum entry by its index.
    pub fn get_enum(&self, entry_no: usize) -> Option<&EnumSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Enum(e) => Some(e),
                _ => None,
            })
            .flatten()
    }
    /// Get a variable entry by its index.
    pub fn get_variable(&self, entry_no: usize) -> Option<&VariableSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Variable(v) => Some(v),
                _ => None,
            })
            .flatten()
    }
    /// Get a variable entry by its index.
    pub fn get_variable_mut(&mut self, entry_no: usize) -> Option<&mut VariableSignature> {
        self.entries
            .get_mut(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Variable(v) => Some(v),
                _ => None,
            })
            .flatten()
    }
    /// Get a scope entry.
    pub fn get_entry(&self, entry_no: usize) -> Option<&ScopeEntry> {
        self.entries.get(entry_no)
    }

    /// Get a scope entry mutably.
    fn get_entry_mut(&mut self, entry_no: usize) -> Option<&mut ScopeEntry> {
        self.entries.get_mut(entry_no)
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
    pub fn create_shadow(&self, current_scope: usize) -> ScopeManagerShadow {
        ScopeManagerShadow {
            base: self,
            current_scope,
        }
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
        scope.find(name).map(|(index, entry)| ScopeSearch {
            entry,
            index,
            scope,
        })
    }
    /// Search for an entry within the current scope, or within any of its ancestors.
    pub fn lookup(&self, name: &str) -> Option<ScopeSearch> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            match scope.find(name) {
                Some((index, entry)) => {
                    return Some(ScopeSearch {
                        entry,
                        index,
                        scope,
                    })
                }
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
            Some((index, entry)) => {
                return Some(ScopeSearch {
                    entry,
                    index,
                    scope,
                })
            }
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
    /// Register a declaration as being present within a scope.
    pub fn register(&mut self, entry: ScopeEntry) -> usize {
        self.scopes[self.current_scope].add(entry)
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
    /// Returns the scope with the given index.
    pub fn get_scope_mut(&mut self, index: usize) -> Option<&mut Scope> {
        self.scopes.get_mut(index)
    }

    /// Returns an entry using a scope address without checks.
    /// Panics if the scope or entry is not found.
    pub fn get_entry_unguarded(&self, address: ScopeAddress) -> &ScopeEntry {
        match self.get_scope(address.scope_id) {
            Some(scope) => match scope.get_entry(address.entry_no) {
                Some(entry) => entry,
                None => panic!("Could not find entry with no {}", address.entry_no),
            },
            None => panic!("Could not find scope with id {}", address.scope_id),
        }
    }

    /// Returns a **mutable** entry using a scope address without checks.
    /// Panics if the scope or entry is not found.
    pub fn get_entry_unguarded_mut(&mut self, address: ScopeAddress) -> &mut ScopeEntry {
        match self.get_scope_mut(address.scope_id) {
            Some(scope) => match scope.get_entry_mut(address.entry_no) {
                Some(entry) => entry,
                None => panic!("Could not find entry with no {}", address.entry_no),
            },
            None => panic!("Could not find scope with id {}", address.scope_id),
        }
    }
}

impl<'a> ScopeManagerShadow<'a> {
    /// Search for an entry within the current scope, or within any of its ancestors.
    pub fn lookaround(&self, name: &str) -> Option<ScopeSearch> {
        let scope = &self.base.scopes[self.current_scope];
        scope.find(name).map(|(index, entry)| ScopeSearch {
            entry,
            index,
            scope,
        })
    }
    /// Search for an entry within the current scope, or within any of its ancestors.
    pub fn lookup(&self, name: &str) -> Option<ScopeSearch> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.base.scopes[current_scope];
            match scope.find(name) {
                Some((index, entry)) => {
                    return Some(ScopeSearch {
                        entry,
                        index,
                        scope,
                    })
                }
                None => match scope.parent_index {
                    Some(parent_index) => current_scope = parent_index,
                    None => return None,
                },
            }
        }
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
