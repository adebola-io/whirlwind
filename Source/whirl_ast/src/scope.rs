use std::collections::HashMap;

use crate::{
    EnumSignature, FunctionSignature, Identifier, ModelSignature, TraitSignature, TypeSignature,
    VariableSignature,
};

/// A hierarchical data structure that stores info in related, ordered "depths".
/// It provides functions for creating and managing the lifecycle of nested scopes.
#[derive(Debug)]
pub struct ScopeManager {
    module_name: Option<Identifier>,
    scopes: Vec<Scope>,
    current_scope: usize,
    last_valid: Option<usize>,
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
    Model(ModelSignature),
    Enum(EnumSignature),
    Variable(VariableSignature),
    Trait(TraitSignature),
    // Imported(ImportedSignature),
}

#[derive(Debug, Default)]
pub enum ScopeType {
    Global,
    Functional,
    Local,
    Test,
    Constructor,
    Method,
    /// A placeholder scope.
    #[default]
    VoidScope,
}

/// The scope address stores the address of a symbol in the scope manager.
#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct ScopeAddress {
    /// The entry in which it is declared.
    pub scope_id: usize,
    /// The entry number.
    pub entry_no: usize,
}

#[derive(Debug, Default)]
pub struct Scope {
    pub id: usize,
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
            | ScopeEntry::Model(ModelSignature { name, .. })
            | ScopeEntry::Enum(EnumSignature { name, .. })
            | ScopeEntry::Variable(VariableSignature { name, .. })
            | ScopeEntry::Trait(TraitSignature { name, .. }) => &name.name,
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
            id: 0,
            _type: ScopeType::Global,
            parent_index: None,
            children_index: vec![],
            entries: vec![],
        }
    }
    /// Creates a child of another scope.
    fn local(id: usize, parent: usize, _type: ScopeType) -> Self {
        Scope {
            id,
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
    /// Remove an entry with an index.
    pub fn remove(&mut self, entry_no: usize) -> ScopeEntry {
        self.entries.remove(entry_no)
    }
    /// Get a model entry by its index.
    pub fn get_model(&self, entry_no: usize) -> Option<&ModelSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Model(c) => Some(c),
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
    /// Get a trait entry by its index.
    pub fn get_trait(&self, entry_no: usize) -> Option<&TraitSignature> {
        self.entries
            .get(entry_no)
            .map(|entry| match entry {
                ScopeEntry::Trait(t) => Some(t),
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
    /// Returns true if the scope is a global scope.
    pub fn is_global(&self) -> bool {
        matches!(self._type, ScopeType::Global)
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
            module_name: None,
            scopes: vec![Scope::global()],
            current_scope: 0,
            last_valid: None,
        }
    }
    /// Create a scope manager for a module.
    pub fn for_module(module_name: Identifier) -> Self {
        ScopeManager {
            module_name: Some(module_name),
            scopes: vec![Scope::global()],
            current_scope: 0,
            last_valid: None,
        }
    }
    /// Checks if the program is currently in the global scope.
    pub fn is_global(&self) -> bool {
        matches!(&self.scopes[self.current_scope]._type, ScopeType::Global)
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
    /// Removes a scope at an index from the program.
    ///
    /// To preserve the hierarchy, the scope and its descendants are moved into a new scope manager.<br>
    ///
    /// After removal, holes are left in the main manager that can be removed with [`ScopeManager::rectify()`].
    pub fn remove(&mut self, id: usize) -> Option<ScopeManager> {
        self.scopes.get(id)?;
        let mut submanager = remove_scope_in_place(self, id);
        submanager.rectify();
        Some(submanager)
    }
    /// Adjusts the indexes of all the scopes present, while removing all holes in the program.
    pub fn rectify(&mut self) {
        // Remove all void scopes.
        self.scopes
            .retain(|scope| !matches!(scope._type, ScopeType::VoidScope));

        // Create old-index -> new-index mapping.
        let mut map = HashMap::<usize, usize>::new();
        self.scopes.iter().enumerate().for_each(|(index, scope)| {
            map.insert(scope.id, index);
        });

        // Reconcile for each scope.
        self.scopes.iter_mut().for_each(|scope| {
            if let Some(parent_id) = scope.parent_index {
                scope.parent_index = map.get(&parent_id).copied();
            }
            scope.id = *map.get(&scope.id).unwrap();
            scope.children_index.sort();
            scope.children_index.iter_mut().for_each(|child_id| {
                *child_id = *map.get(&child_id).unwrap();
            })
        });
        // Back to top.
        self.current_scope = 0;
        self.last_valid = None;
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
    /// Panics if the current scope is the first scope.
    pub fn leave(&mut self) {
        if self.current_scope == 0 {
            panic!("Cannot leave first scope.")
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
    /// Returns the module name of the scope, if it exists.
    pub fn get_module_name(&self) -> Option<&str> {
        self.module_name.as_ref().map(|x| x.name.as_str())
    }
    /// Set the name of the scope.
    pub fn set_name(&mut self, name: Identifier) {
        self.module_name = Some(name);
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
    /// It takes a new scope manager and moves it into the holes that exist in the first.
    /// Basically reverses a removal.
    pub fn merge(&mut self, submanager: &mut ScopeManager) {
        // Ensure that manager is not rectified.
        if self.last_valid.is_none() {
            panic!("Scope manager is already in a rectified state.");
        }
        // Gather all holes.
        let holes = self
            .scopes
            .iter()
            .enumerate()
            .filter(|scope| matches!(scope.1._type, ScopeType::VoidScope))
            .map(|tuple| tuple.0)
            .collect::<Vec<_>>();
        let offset = self.scopes.len();
        self.remove(self.last_valid.unwrap()); // I have no idea.
        let first = submanager.scopes.first_mut();
        if let Some(first_sub_scope) = first {
            first_sub_scope.parent_index = self.last_valid;
            first_sub_scope.id = first_sub_scope.id + offset;
            first_sub_scope
                .children_index
                .iter_mut()
                .for_each(|chid| *chid = *chid + offset);
            // Join downwards
            self.scopes[self.last_valid.unwrap()]
                .children_index
                .push(first_sub_scope.id);
        }
        // Shift all indexes to end of main manager.
        submanager.scopes.iter_mut().skip(1).for_each(|scope| {
            scope.parent_index = scope
                .parent_index
                .map(|pid| pid + offset)
                .or(self.last_valid);
            scope.id = scope.id + self.scopes.len();

            scope
                .children_index
                .iter_mut()
                .for_each(|chid| *chid = *chid + offset);
        });
        // fill holes.
        for hole in holes {
            let scope_to_inject = submanager.scopes.remove(0);
            self.scopes[hole] = scope_to_inject;
        }
        // Add rest values.
        // Shift all indexes to end of main manager.
        while !submanager.scopes.is_empty() {
            self.scopes.push(submanager.scopes.remove(0));
        }
        self.rectify();
    }

    /// Checks up the scope tree to see if the current scope is within a function or method.
    pub fn is_in_function_context(&self) -> bool {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            if matches!(scope._type, ScopeType::Functional | ScopeType::Method) {
                return true;
            } else if let Some(parent) = scope.parent_index {
                current_scope = parent;
            } else {
                return false;
            }
        }
    }

    pub fn is_test(&self) -> bool {
        matches!(&self.scopes[self.current_scope]._type, ScopeType::Test)
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

/// Removes a scope and all its children.
/// It creates a new uncollapsed scope manager for the unedited scope tree.
fn remove_scope_in_place(manager: &mut ScopeManager, scope_id: usize) -> ScopeManager {
    let mut scope = std::mem::take(&mut manager.scopes[scope_id]);
    // Remove from parent.
    if let Some(parent) = scope.parent_index {
        let parent_scope = &mut manager.scopes[parent];
        if !matches!(parent_scope._type, ScopeType::VoidScope) {
            parent_scope
                .children_index
                .retain(|index| *index != scope_id);
            manager.last_valid = Some(parent_scope.id);
        }
    }
    let children_index = std::mem::take(&mut scope.children_index);

    let mut sub_manager = ScopeManager {
        module_name: None,
        scopes: vec![scope],
        current_scope: 0,
        last_valid: None,
    };

    // Remove its children.
    for child_index in children_index {
        let mut sub_sub_manager = remove_scope_in_place(manager, child_index);
        sub_manager.scopes.append(&mut sub_sub_manager.scopes);
    }
    sub_manager
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

    #[test]
    fn test_removal() {
        let mut scope_manager = ScopeManager::new();

        scope_manager.enter(ScopeType::Local); // create scope 1 as child of 0
        scope_manager.enter(ScopeType::Method); // create scope 2 as child of 1
        scope_manager.enter(ScopeType::Test); // create scope 3 as child of 2
        scope_manager.leave(); // back to scope 2
        scope_manager.enter(ScopeType::Local); // create scope 4 as child of 2
        scope_manager.enter(ScopeType::Constructor); // create scope 5 as child of 4.

        let mut fragment = scope_manager.remove(3).unwrap();
        println!("REMOVED: {:#?}\n\n\n", fragment);
        fragment.enter(ScopeType::Functional); // create scope 6 as child of (former) 3
        fragment.enter(ScopeType::Test); // create scope 7 as child of 6
        println!("REMOVED AND EDITED: {:#?}\n\n\n", fragment);

        println!(
            "AFTER REMOVAL: {:#?}\n\n\n The last_valid was {:?}\n\n\n",
            scope_manager, scope_manager.last_valid
        );
        scope_manager.merge(&mut fragment);

        println!("AFTER MERGING: {:#?}\n\n\n", scope_manager);
    }
}
