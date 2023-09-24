use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use whirl_errors::ResolveError;

use crate::Module;
use whirl_utils::get_parent_dir;

/// Representation of all imports and the files and modules they represent.
#[derive(Debug)]
pub struct ModuleGraph {
    root_folder: Option<PathBuf>,
    paths: HashMap<PathBuf, usize>,
    directories: HashMap<PathBuf, HashMap<String, usize>>,
    modules: Vec<Module>,
    pub errors: Vec<ResolveError>,
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
        }
    }
    /// Add a module to the graph and returns its module number.
    pub fn add_module(&mut self, module: Module) -> Option<usize> {
        let module_number = module.module_id;
        let module_path = module.module_path.as_ref()?.to_owned();
        let module_name = module.name.as_ref()?.to_string();
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
        dir_map.insert(module_name, module_number);
        // // Mark global path.
        self.paths.insert(module_path, module_number);
        self.modules.push(module);

        return Some(module_number);
    }
    /// Add an error to the graph.
    pub fn add_error(&mut self, error: ResolveError) {
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
            self.add_error(whirl_errors::io_error(read_dir.err().unwrap()));
            return module_ids;
        }
        let read_dir = read_dir.unwrap();
        for dir_entry_result in read_dir {
            if dir_entry_result.is_err() {
                self.add_error(whirl_errors::io_error(dir_entry_result.err().unwrap()));
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
                let id = self.modules.len();
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
    /// Returns a module at a specified path.
    pub fn get_module_at_path(&self, path: &Path) -> Option<&Module> {
        self.modules.get(*(self.paths.get(path)?))
    }
    /// Returns a mutable reference to a module at a specified path.
    pub fn get_mut_module_at_path(&mut self, path: &Path) -> Option<&mut Module> {
        self.modules.get_mut(*(self.paths.get(path)?))
    }
    /// Set the entry module.
    pub fn set_start(&mut self, module: Module) {
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
}
