use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
};

use whirl_errors::ResolveError;

use crate::Module;

/// Representation of all imports and the files and modules they represent.
#[derive(Debug)]
pub struct ModuleGraph {
    paths: RefCell<HashMap<PathBuf, usize>>,
    directories: HashMap<PathBuf, HashMap<String, usize>>,
    modules: Vec<Module>,
    pub errors: Vec<ResolveError>,
}

impl ModuleGraph {
    /// Create a new module graph.
    pub fn new() -> Self {
        ModuleGraph {
            paths: RefCell::new(HashMap::new()),
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
        let module_directory = get_dir_folder(&module_path)?;
        let directories = &mut self.directories;

        // println!("Adding module {module_number}, {module_name}");

        // Mark directory module.
        let dir_map = match directories.get_mut(module_directory) {
            Some(dir_map) => dir_map,
            None => {
                directories.insert(module_directory.to_path_buf(), HashMap::new());
                directories.get_mut(module_directory).unwrap()
            }
        };
        dir_map.insert(module_name, module_number);
        // Mark global path.
        self.paths().insert(module_path, module_number);
        self.modules.push(module);

        return Some(module_number);
    }
    /// Add an error to the graph.
    pub fn add_error(&mut self, error: ResolveError) {
        self.errors.push(error)
    }
    /// Returns the paths.
    pub fn paths(&self) -> &mut HashMap<PathBuf, usize> {
        unsafe { &mut (*self.paths.as_ptr()) }
    }
    /// Returns true if there are no modules in the graph.
    pub fn is_empty(&self) -> bool {
        self.modules.len() == 0
    }

    /// Build the graph, starting from the entry.
    pub fn unravel(&mut self) -> Option<()> {
        let enclosing_directory =
            get_dir_folder(self.modules.first()?.module_path.as_ref()?)?.to_path_buf();

        let skip = Some(self.modules.first()?.module_path.as_ref()?.to_owned());
        self.add_modules_in_dir(&enclosing_directory, skip);
        let entry_module = self.modules.first()?;
        let imports = entry_module.get_use_imports();
        // println!(
        //     "In directory, {:?}, {:?}",
        //     enclosing_directory,
        //     self.directories.get(&enclosing_directory)
        // );
        for import in imports {
            let module = self.get_module_in_dir(&enclosing_directory, &import.target.name.name);
            println!("Module {}: {:#?}\n\n", import.target.name.name, module)
        }
        // println!("{:#?}", self.directories);
        Some(())
    }
    /// Returns a list of the ids of the modules in a particular directory.
    /// If there are modules in the directory that have not been traversed,
    /// they will be added to the graph.
    pub fn add_modules_in_dir(&mut self, dir: &Path, skipover: Option<PathBuf>) -> Vec<usize> {
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
                let already_traversed = self.paths().get(&path);
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
                if let Some(id) = self
                    .add_modules_in_dir(&path, None)
                    .iter()
                    .find(|module_id| {
                        self.modules[**module_id]
                            .name
                            .as_ref()
                            .is_some_and(|name| name.as_str() == base_name)
                    })
                {
                    module_ids.push(*id);
                }
            }
        }
        module_ids
    }
    /// Return a module in a directory, only if it has been traversed.
    pub fn get_module_in_dir(&self, dir: &Path, name: &str) -> Option<&Module> {
        let module_id = self.directories.get(dir)?.get(name).or_else(|| {
            // Find name in the module/name directory
            let mut nested = dir.to_path_buf();
            nested.push(name);
            self.directories.get(&nested)?.get(name)
        })?;
        self.modules.get(*module_id)
    }
}

fn get_dir_folder(module_path: &PathBuf) -> Option<&Path> {
    module_path.ancestors().nth(1)
}
