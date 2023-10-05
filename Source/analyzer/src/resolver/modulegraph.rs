use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use ast::{UsePath, UseTarget};
use errors::ImportError;

use crate::Module;
use utils::get_parent_dir;

/// Representation of all imports and the files and modules they represent.
#[derive(Debug)]
pub struct ModuleGraph {
    pub root_folder: Option<PathBuf>,
    pub paths: HashMap<PathBuf, usize>,
    pub directories: HashMap<PathBuf, HashMap<String, usize>>,
    pub modules: Vec<Module>,
    holes: Vec<usize>,
    pub errors: Vec<ImportError>,
}

impl ModuleGraph {
    /// Create a new module graph.
    pub fn new() -> Self {
        ModuleGraph {
            root_folder: None,
            paths: HashMap::new(),
            directories: HashMap::new(),
            modules: vec![],
            errors: vec![],
            holes: vec![],
        }
    }
    /// Add a module to the graph and returns its module number.
    pub fn add_module(&mut self, module: Module) -> Option<usize> {
        let module_number = module.module_id;
        let module_path = module.module_path.as_ref()?.to_owned();
        let module_directory = get_parent_dir(&module_path)?;
        let directories = &mut self.directories;
        // // Mark directory module.
        let dir_map = match directories.get_mut(module_directory) {
            Some(dir_map) => dir_map,
            None => {
                directories.insert(module_directory.to_path_buf(), HashMap::new());
                directories.get_mut(module_directory).unwrap()
            }
        };
        // Give module an anonymous name so it can remain tracked.
        let module_name = match module.name {
            Some(ref name) => name.to_string(),
            None => {
                let mut anonymous_name = module_path.file_stem()?.to_str()?.to_string();
                while dir_map.get(&anonymous_name).is_some() {
                    anonymous_name.push_str("_x")
                }
                anonymous_name
            }
        };
        dir_map.insert(module_name, module_number);
        // // Mark global path.
        self.paths.insert(module_path, module_number);
        if module_number < self.modules.len() {
            self.modules[module_number] = module;
        } else {
            self.modules.push(module);
        }
        return Some(module_number);
    }
    /// Check a module to confirm that all its imports are valid.
    pub fn audit_module(&self, module: &Module) -> Option<Vec<ImportError>> {
        let imports = module.get_use_imports();
        let parent_dir = get_parent_dir(module.module_path.as_ref()?)?;
        let mut errors = vec![];
        // Module has no name.
        if module.name.is_none() {
            errors.push(errors::nameless_module());
        }
        for import in imports {
            self.audit_import_target(&mut errors, module, parent_dir, &import.target);
        }

        Some(errors)
    }
    fn audit_import_target(
        &self,
        errors: &mut Vec<ImportError>,
        start_module: &Module,
        dir: &Path,
        target: &UseTarget,
    ) {
        // Check that module import exists.
        match self.get_module_in_dir(dir, &target.name.name) {
            None => errors.push(errors::cannot_find_module(
                target.name.name.to_owned(),
                target.name.span,
            )),
            Some(imported_module) => {
                // Block importing a module into itself.
                if std::ptr::eq(imported_module, start_module) {
                    errors.push(errors::self_import(
                        target.name.name.to_owned(),
                        target.name.span,
                    ))
                }
                // Check descendant imports.
                self.audit_import_paths(errors, imported_module, start_module, &target.path);
            }
        }
    }
    /// Check that the sub imports are correct.
    fn audit_import_paths(
        &self,
        errors: &mut Vec<ImportError>,
        imported_module: &Module,
        start_module: &Module,
        path: &UsePath,
    ) {
        let targets = match path {
            // Module has already been checked.
            UsePath::Me => return,
            UsePath::Item(target) => vec![target.as_ref()],
            UsePath::List(targets) => targets.iter().collect(),
        };
        for target in targets {
            // Search global scope for item.
            let shadow = imported_module.ambience.create_shadow(0);
            if let Some(search) = shadow.lookaround(&target.name.name) {
                // Importing non public symbol.
                if !search.entry.is_public() {
                    errors.push(errors::private_symbol_leak(
                        imported_module.name.clone().unwrap(),
                        target.name.to_owned(),
                    ))
                }
                if let ast::ScopeEntry::UseImport(_) = search.entry {
                    // import indirection.
                    if !matches!(target.path, UsePath::Me) {
                        let parent_dir =
                            get_parent_dir(imported_module.module_path.as_ref().unwrap()).unwrap(); // todo: when will this panic?
                        self.audit_import_target(errors, start_module, parent_dir, target);
                    }
                } else {
                    // If path is still going, return error.
                    if !matches!(target.path, UsePath::Me) {
                        errors.push(errors::symbol_not_a_module(target.name.to_owned()))
                    }
                }
            } else {
                if imported_module.name.is_some() {
                    errors.push(errors::no_such_symbol_in_module(
                        imported_module.name.clone().unwrap(),
                        target.name.to_owned(),
                    ))
                }
            }
        }
    }
    /// Add an error to the graph.
    pub fn add_error(&mut self, error: ImportError) {
        self.errors.push(error)
    }
    /// Returns true if there are no modules in the graph.
    pub fn is_empty(&self) -> bool {
        self.modules.len() == 0
    }
    /// Build the graph, starting from the entry.
    pub fn unravel(&mut self) -> Option<()> {
        let enclosing_directory =
            get_parent_dir(self.modules.first()?.module_path.as_ref()?)?.to_path_buf();
        // Skip over the entry module instead of parsing twice.
        let skip = Some(self.modules.first()?.module_path.as_ref()?.to_owned());
        // Add modules downwards the module tree.
        self.traverse_dir(&enclosing_directory, skip);
        self.refresh();
        Some(())
    }
    /// Dives into a directory and traverses all files and folders at all sub levels.
    /// If there are modules in the directory that have not been traversed,
    /// they will be added to the graph.
    /// Returns a list of the ids of the modules one level deep in a particular directory.
    pub fn traverse_dir(&mut self, dir: &Path, skipover: Option<PathBuf>) -> Vec<usize> {
        let mut module_ids = vec![];
        let read_dir = dir.read_dir();
        if read_dir.is_err() {
            self.add_error(errors::io_error(read_dir.err().unwrap()));
            return module_ids;
        }
        let read_dir = read_dir.unwrap();
        for dir_entry_result in read_dir {
            if dir_entry_result.is_err() {
                self.add_error(errors::io_error(dir_entry_result.err().unwrap()));
                continue;
            }
            let path = dir_entry_result.unwrap().path();
            if skipover
                .as_ref()
                .is_some_and(|skip_path| *skip_path == path)
            {
                continue;
            }
            if path.is_file() && path.extension().is_some_and(|ext| ext == "wrl") {
                // Check to see if its already added.
                let already_traversed = self.paths.get(&path);
                if let Some(id) = already_traversed {
                    module_ids.push(*id);
                    continue;
                }
                let id = self.holes.pop().unwrap_or(self.modules.len());
                match Module::from_path(path, id) {
                    Ok(module) => {
                        if let Some(id) = self.add_module(module) {
                            module_ids.push(id)
                        }
                    }
                    Err(error) => self.add_error(error),
                }
            } else if path.is_dir() {
                let base_name = path.file_stem();
                if base_name.is_none() {
                    continue;
                }
                let base_name = base_name.unwrap();
                if let Some(id) = self.traverse_dir(&path, None).iter().find(|module_id| {
                    self.modules[**module_id]
                        .name
                        .as_ref()
                        .is_some_and(|name| name.as_str() == base_name)
                }) {
                    module_ids.push(*id);
                }
            }
        }
        module_ids
    }
    /// Return a module in a directory, only if it has been traversed.
    pub fn get_module_in_dir(&self, dir: &Path, name: &str) -> Option<&Module> {
        // Look one level up for super.
        let (directory, name) = if name == "Super" {
            let parent_dir = get_parent_dir(dir)?;
            // Returning the package itself from a Super.
            if parent_dir == self.root_folder.as_ref()? {
                return self.modules.first();
            }
            (parent_dir, dir.file_stem()?.to_str()?)
        } else if name == "Package" {
            // Returning the start module.
            return self.modules.first();
        } else {
            (dir, name)
        };
        let module_id = self.directories.get(directory)?.get(name).or_else(|| {
            // Find name in the module/name directory
            let mut nested = dir.to_path_buf();
            nested.push(name);
            self.directories.get(&nested)?.get(name)
        })?;
        self.modules.get(*module_id)
    }
    /// Returns the index of the module in a directory, only if it has been traversed.
    pub fn get_module_id_in_dir(&self, dir: &Path, name: &str) -> Option<usize> {
        // Look one level up for super.
        let (directory, name) = if name == "Super" {
            let parent_dir = get_parent_dir(dir)?;
            // Returning the package itself from a Super.
            if parent_dir == self.root_folder.as_ref()? {
                return Some(0);
            }
            (parent_dir, dir.file_stem()?.to_str()?)
        } else if name == "Package" {
            // Returning the start module.
            return Some(0);
        } else {
            (dir, name)
        };
        let module_id = self.directories.get(directory)?.get(name).or_else(|| {
            // Find name in the module/name directory
            let mut nested = dir.to_path_buf();
            nested.push(name);
            self.directories.get(&nested)?.get(name)
        })?;
        Some(*module_id)
    }
    /// Returns a module at a specified path.
    pub fn get_module_at_path(&self, path: &Path) -> Option<&Module> {
        self.modules.get(*(self.paths.get(path)?))
    }
    /// Returns a mutable reference to a module at a specified path.
    pub fn get_mut_module_at_path(&mut self, path: &Path) -> Option<&mut Module> {
        self.modules.get_mut(*(self.paths.get(path)?))
    }
    /// Returns a mutable reference to a module with a given id.
    pub fn get_module_with_id(&self, id: usize) -> Option<&Module> {
        self.modules.get(id)
    }
    /// Create a pathway of modules from a global directory module to this module,
    /// and then store the line in a vector.
    pub fn draw_line_to(&self, module: &Module, pathway: &mut Vec<usize>) {
        // println!("{:?}, {:?}", module.module_id, module.name);
        // Add this module.
        pathway.insert(0, module.module_id);
        // Add the directory above.
        let directory_above = (|| -> Option<(&Path, &str)> {
            let mut parent_directory = get_parent_dir(module.module_path.as_ref()?)?;
            let mut parent_module_name = parent_directory.file_stem()?.to_str()?;
            // prevent infinite loop for current dir;
            if module.name.as_ref()? == parent_module_name {
                parent_directory = get_parent_dir(parent_directory)?;
                parent_module_name = parent_directory.file_stem()?.to_str()?;
            }
            Some((parent_directory, parent_module_name))
        })();
        if let Some(directory_above) = directory_above {
            if let Some(parent_module) =
                self.get_module_in_dir(directory_above.0, directory_above.1)
            {
                self.draw_line_to(parent_module, pathway);
            }
        }
    }
    /// Set the entry module.
    pub fn set_entry_module(&mut self, module: Module) {
        self.root_folder = module
            .module_path
            .as_ref()
            .and_then(|path| get_parent_dir(path))
            .map(|path| path.to_path_buf());

        self.add_module(module);
    }
    /// Returns true if a file is part of this import graph.
    pub fn contains_file(&self, path: &Path) -> bool {
        self.paths.get(path).is_some()
    }
    /// Returns true if a folder is contained in the graph.
    pub fn contains_folder(&self, dir: &Path) -> bool {
        self.directories.get(dir).is_some()
    }
    /// Returns the number of modules in the graph.
    pub fn len(&self) -> usize {
        self.modules.len()
    }
    /// Return an iterator over the modules.
    pub fn modules(&self) -> impl Iterator<Item = &Module> {
        self.modules.iter()
    }
    /// Return a mutable iterator over the modules.
    pub fn modules_mut(&mut self) -> std::slice::IterMut<Module> {
        self.modules.iter_mut()
    }
    /// Removes a folder and all its modules.
    pub fn remove_folder(&mut self, path: &Path) -> Option<()> {
        let folder = self.directories.get(path)?;
        let affected_modules_idx = folder.iter().map(|tuple| *tuple.1);

        for module_idx in affected_modules_idx {
            self.paths
                .remove::<PathBuf>(self.modules.get(module_idx)?.module_path.as_ref()?);
            // Empty placeholder module.
            let mut module = Module::default();
            module.module_id = module_idx;
            module.ambience.module_id = module_idx;
            self.modules[module_idx] = module;
            self.holes.push(module_idx);
        }
        self.directories.remove(path);
        if self
            .root_folder
            .as_ref()
            .is_some_and(|ref folder| *folder == path)
        {
            self.root_folder = None;
        }
        // Refresh graph.
        self.refresh();
        Some(())
    }
    /// Refresh the graph.
    pub fn refresh(&mut self) {
        let mut errors = vec![];
        // Hanlde module renaming.
        self.modules.iter_mut().for_each(|module| {
            if module.module_path.is_none() || module.name.is_none() {
                return;
            }
            let module_name = module.name.as_ref().unwrap();
            get_parent_dir(module.module_path.as_ref().unwrap())
                .and_then(|directory| self.directories.get_mut(directory))
                .map(|dir_map| {
                    if !dir_map
                        .get(module_name)
                        .is_some_and(|id| *id == module.module_id)
                    {
                        let mut old_name = None;
                        // find the old name.
                        for (name, id) in dir_map.iter() {
                            if *id == module.module_id {
                                old_name = Some(name.to_owned());
                                break;
                            }
                        }
                        dir_map.remove(&old_name.unwrap());
                        // add new name.
                        dir_map.insert(module_name.to_string(), module.module_id);
                    }
                });
        });
        self.modules.iter().for_each(|module| {
            errors.push(self.audit_module(module));
        });
        self.modules.iter_mut().rev().for_each(|module| {
            module.set_resolve_errors(errors.pop().unwrap());
        });
    }
    /// Removes a module from the program.
    pub fn remove_module_at_path(&mut self, path: &Path) -> Option<()> {
        let parent_dir = get_parent_dir(path)?;
        let module_idx = *self.paths.get(path)?;
        let module_name = self.modules[module_idx].name.as_ref()?;

        // Remove from directory map.
        let directory_map = &mut self.directories;
        let directory = directory_map.get_mut(parent_dir)?;
        directory.remove(module_name);

        self.paths.remove(path);
        // Empty placeholder module.
        let mut module = Module::default();
        module.module_id = module_idx;
        module.ambience.module_id = module_idx;
        self.modules[module_idx] = module;
        self.holes.push(module_idx);

        self.refresh();
        Some(())
    }
}
