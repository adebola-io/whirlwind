use std::collections::HashMap;

use crate::{Identifier, Scope, ScopeAddress, ScopeEntry, ScopeSearch, ScopeType};

/// ### The Environment of a Module.
/// This struct provides a hierarchical data structure that stores info in related, ordered "depths".
/// It provides functions for creating and managing the lifecycle of nested scopes,
/// as well as navigating through the context of other modules and packages in
/// a project.
#[derive(Debug)]
pub struct ModuleAmbience {
    pub module_id: usize,
    pub module_name: Option<Identifier>,
    pub module_info: Option<Vec<String>>,
    scopes: Vec<Scope>,
    current_scope: usize,
}

/// A shallow copy of the module ambience with a different current scope.
/// Allows for lookup without leaving the current scope.
pub struct ModuleAmbienceShadow<'a> {
    base: &'a ModuleAmbience,
    current_scope: usize,
}

impl Default for ModuleAmbience {
    fn default() -> Self {
        ModuleAmbience::new(0)
    }
}

impl ModuleAmbience {
    /// Create a new module ambience.
    pub fn new(module_id: usize) -> Self {
        ModuleAmbience {
            module_id,
            module_name: None,
            module_info: None,
            scopes: vec![Scope::global()],
            current_scope: 0,
        }
    }
    /// Create a module ambience for a module.
    pub fn for_module(module_name: Identifier, module_id: usize) -> Self {
        ModuleAmbience {
            module_id,
            module_name: Some(module_name),
            module_info: None,
            scopes: vec![Scope::global()],
            current_scope: 0,
        }
    }
    /// Checks if the program is currently in the global scope.
    pub fn is_in_global_scope(&self) -> bool {
        matches!(&self.scopes[self.current_scope]._type, ScopeType::Global)
    }
    /// Checks if the current scope is a test scope.
    pub fn is_in_test_scope(&self) -> bool {
        matches!(&self.scopes[self.current_scope]._type, ScopeType::Test)
    }
    /// Checks up the scope tree to see if the current scope is within a function or method.
    pub fn is_in_function_context(&self) -> bool {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            if matches!(
                scope._type,
                ScopeType::Functional
                    | ScopeType::ModelMethodOf { .. }
                    | ScopeType::InterfaceMethodOf { .. }
            ) {
                return true;
            } else if let Some(parent) = scope.parent_index {
                current_scope = parent;
            } else {
                return false;
            }
        }
    }
    /// Checks up the scope tree to see if the current scope is within a method of a interface or model.
    pub fn is_in_method_context(&self) -> bool {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            if matches!(
                scope._type,
                ScopeType::ModelMethodOf { .. } | ScopeType::InterfaceMethodOf { .. }
            ) {
                return true;
            } else if let Some(parent) = scope.parent_index {
                current_scope = parent;
            } else {
                return false;
            }
        }
    }
    pub fn create_shadow(&self, current_scope: usize) -> ModuleAmbienceShadow {
        ModuleAmbienceShadow {
            base: self,
            current_scope,
        }
    }
    /// Returns the number of scopes in the module.
    pub fn scope_len(&self) -> usize {
        self.scopes.len()
    }
    /// Returns the number of current nested scopes.
    pub fn current_scope_depth(&self) -> usize {
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
    pub fn current_scope(&self) -> usize {
        self.current_scope
    }
    /// Goes to a new current scope.
    pub fn jump_to_scope(&mut self, scope_id: usize) {
        self.current_scope = scope_id;
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
    /// To preserve the hierarchy, the scope and its descendants are moved into a new module ambience.<br>
    ///
    /// After removal, holes are left in the main manager that can be removed with [`ModuleAmbience::rectify_scopes()`].
    pub fn remove_scope(&mut self, id: usize) -> Option<ModuleAmbience> {
        self.scopes.get(id)?;
        let mut submanager = remove_scope_in_place(self, id);
        submanager.rectify_scopes();
        Some(submanager)
    }
    /// Adjusts the indexes of all the scopes present, while removing all holes in the program.
    pub fn rectify_scopes(&mut self) {
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
    pub fn leave_scope(&mut self) {
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
    pub fn set_module_name(&mut self, name: Identifier) {
        self.module_name = Some(name);
    }
    /// Checks up the scope tree to retrieve the meaning of `this`,
    /// which could either be a interface or a model.
    pub fn get_method_context(&self) -> Option<ScopeSearch> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            match scope._type {
                ScopeType::ModelConstructorOf { model: address }
                | ScopeType::ModelMethodOf { model: address }
                | ScopeType::InterfaceMethodOf {
                    _interface: address,
                } => {
                    let entry = self.get_entry_unguarded(address);
                    return Some(ScopeSearch {
                        index: address.entry_no,
                        entry,
                        scope,
                    });
                }
                _ => {
                    if let Some(parent) = scope.parent_index {
                        current_scope = parent;
                    } else {
                        return None;
                    }
                }
            }
        }
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
    /// Returns the id of the module.
    pub fn id(&self) -> usize {
        self.module_id
    }
    /// Reserve the next scope entry for an atom that has not yet been parsed.
    pub fn reserve_entry_space(&mut self) -> usize {
        self.scopes[self.current_scope].add(ScopeEntry::ReservedSpace)
    }
    /// Register an entry at a previously reserved entry number. It panics if the entry is already allocated.
    #[track_caller]
    pub fn register_at(&mut self, entry_no: usize, signature: ScopeEntry) {
        match self.scopes[self.current_scope].get_entry_mut(entry_no) {
            Some(entry) if entry.is_reserved() => *entry = signature,
            Some(entry) => panic!("{entry_no} is already allocated to {}", entry.name()),
            None => panic!("Cannot allocate to an out of bounds entry number: {entry_no}"),
        }
    }
    /// Returns true if the current scope or any of the parent scopes is a for or while loop.
    pub fn is_in_loop_context(&self) -> bool {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.scopes[current_scope];
            match scope._type {
                ScopeType::ForLoop | ScopeType::WhileLoop => return true,
                _ => {
                    if let Some(parent) = scope.parent_index {
                        current_scope = parent;
                    } else {
                        return false;
                    }
                }
            }
        }
    }
}

impl<'a> ModuleAmbienceShadow<'a> {
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
    /// Checks up the scope tree to retrieve the meaining of `this`,
    /// which could either be a interface or a model.
    pub fn get_method_context(&self) -> Option<ScopeSearch> {
        let mut current_scope = self.current_scope;
        loop {
            let scope = &self.base.scopes[current_scope];
            match scope._type {
                ScopeType::ModelConstructorOf { model: address }
                | ScopeType::ModelMethodOf { model: address }
                | ScopeType::InterfaceMethodOf {
                    _interface: address,
                } => {
                    let entry = self.base.get_entry_unguarded(address);
                    return Some(ScopeSearch {
                        index: address.entry_no,
                        entry,
                        scope,
                    });
                }
                _ => {
                    if let Some(parent) = scope.parent_index {
                        current_scope = parent;
                    } else {
                        return None;
                    }
                }
            }
        }
    }
    /// Returns true if the current scope is nested within another.
    pub fn is_inclusive_child_of(&self, parent: usize) -> bool {
        if parent == self.current_scope {
            return true;
        }
        let possible_ancestor = &self.base.scopes[parent];
        for child_idx in &possible_ancestor.children_index {
            if self.is_inclusive_child_of(*child_idx) {
                return true;
            }
        }
        return false;
    }

    /// Checks if the program is currently in the global scope.
    pub fn is_in_global_scope(&self) -> bool {
        matches!(
            &self.base.scopes[self.current_scope]._type,
            ScopeType::Global
        )
    }
}

/// Removes a scope and all its children.
/// It creates a new uncollapsed module ambience for the unedited scope tree.
fn remove_scope_in_place(manager: &mut ModuleAmbience, scope_id: usize) -> ModuleAmbience {
    let mut scope = std::mem::take(&mut manager.scopes[scope_id]);
    // Remove from parent.
    if let Some(parent) = scope.parent_index {
        let parent_scope = &mut manager.scopes[parent];
        if !matches!(parent_scope._type, ScopeType::VoidScope) {
            parent_scope
                .children_index
                .retain(|index| *index != scope_id);
        }
    }
    let children_index = std::mem::take(&mut scope.children_index);

    let mut sub_manager = ModuleAmbience {
        module_id: manager.module_id,
        module_info: None,
        module_name: None,
        scopes: vec![scope],
        current_scope: 0,
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
    use crate::{scope::ScopeType, ModuleAmbience};

    #[test]
    fn test_depth() {
        let mut ambience = ModuleAmbience::new(0);

        assert_eq!(ambience.current_scope_depth(), 0);

        ambience.enter(ScopeType::Local);
        ambience.enter(ScopeType::Local);
        assert_eq!(ambience.current_scope_depth(), 2);

        ambience.enter(ScopeType::Local);
        ambience.enter(ScopeType::Local);
        assert_eq!(ambience.current_scope_depth(), 4);

        ambience.leave_scope();
        assert_eq!(ambience.current_scope_depth(), 3);

        ambience.leave_scope();
        ambience.leave_scope();
        assert_eq!(ambience.current_scope_depth(), 1);
    }

    #[test]
    fn test_removal() {
        let mut module_ambience = ModuleAmbience::new(0);

        module_ambience.enter(ScopeType::Local); // create scope 1 as child of 0
        module_ambience.enter(ScopeType::ModelMethodOf {
            model: [1, 1, 1].into(),
        }); // create scope 2 as child of 1
        module_ambience.enter(ScopeType::Test); // create scope 3 as child of 2
        module_ambience.leave_scope(); // back to scope 2
        module_ambience.enter(ScopeType::Local); // create scope 4 as child of 2
        module_ambience.enter(ScopeType::ModelConstructorOf {
            model: [2, 3, 2].into(),
        }); // create scope 5 as child of 4.

        let mut fragment = module_ambience.remove_scope(3).unwrap();
        println!("REMOVED: {:#?}\n\n\n", fragment);
        fragment.enter(ScopeType::Functional); // create scope 6 as child of (former) 3
        fragment.enter(ScopeType::Test); // create scope 7 as child of 6
        println!("REMOVED AND EDITED: {:#?}\n\n\n", fragment);
    }
}
