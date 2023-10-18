use super::{symbols::*, ProgramError};
use crate::{
    bind, Module, ModuleMap, PathIndex,
    ProgramErrorType::{self, Importing},
    TypedModule,
};
use ast::UseTarget;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use utils::get_parent_dir;

/// A fully resolved representation of an entire program.
#[derive(Debug)]
pub struct Standpoint {
    pub module_map: ModuleMap,
    pub root_folder: Option<PathBuf>,
    pub entry_module: PathIndex,
    pub directories: HashMap<PathBuf, HashMap<String, PathIndex>>,
    pub symbol_table: SymbolTable,
    pub literals: Vec<Literal>,
    pub errors: Vec<ProgramError>,
    pub should_resolve_imports: bool,
    // The path to the entry module of the core library.
    pub corelib_path: Option<PathIndex>,
    // pub warnings: Vec<Warnings>,
}

impl Standpoint {
    /// Creates a new standpoint.
    pub fn new(should_resolve_imports: bool, corelib_path: Option<PathBuf>) -> Self {
        let mut standpoint = Standpoint {
            module_map: ModuleMap::new(),
            entry_module: PathIndex(0),
            directories: HashMap::new(),
            symbol_table: SymbolTable::new(),
            literals: vec![],
            errors: vec![],
            root_folder: None,
            should_resolve_imports,
            corelib_path: None,
        };
        //todo: if corelib is already loaded in another standpoint.
        if let Some(path) = corelib_path {
            match Module::from_path(path) {
                Ok(module) => standpoint.corelib_path = standpoint.add_module(module),
                Err(error) => standpoint.add_import_error(error),
            }
        }
        standpoint
    }
    /// Builds a program standpoint from the entry module.
    /// It also specifies whether the module imports should be resolved, which adds multiple modules to the standpoint.
    pub fn build_from_module(module: Module, should_resolve_imports: bool) -> Option<Self> {
        let mut standpoint = Standpoint::new(should_resolve_imports, None);
        standpoint.add_module(module);
        Some(standpoint)
    }
    /// Returns the first declaration of a symbol.
    pub fn get_declaration_of(&self, index: SymbolIndex) -> Option<SemanticSymbolDeclaration> {
        let symbol = self.symbol_table.get(index)?;
        let first_reference = symbol.references.first()?;
        Some(SemanticSymbolDeclaration {
            module_path: &self.module_map.get(first_reference.module_path)?.path_buf,
            span: &symbol.origin_span,
        })
    }
    /// Find all references to a symbol using its index.
    pub fn find_all_references(
        &self,
        index: SymbolIndex,
    ) -> Option<impl Iterator<Item = SemanticSymbolReference>> {
        Some(
            self.symbol_table
                .get(index)?
                .references
                .iter()
                .map(|list| {
                    list.starts
                        .iter()
                        .map(|start_position| SemanticSymbolReference {
                            module_path: &self.module_map.get(list.module_path).unwrap().path_buf,
                            start_position: *start_position,
                        })
                })
                .flatten(),
        )
    }
    /// Add a module to the standpoint and returns its path index.
    pub fn add_module(&mut self, module: Module) -> Option<PathIndex> {
        let module_path = module.module_path.as_ref()?.to_owned();
        let module_directory = get_parent_dir(&module_path)?;
        let directories = &mut self.directories;
        let module_ident_span = module.ambience.module_name.as_ref()?.span;
        // Mark directory module.
        let dir_map = match directories.get_mut(module_directory) {
            Some(dir_map) => dir_map,
            None => {
                directories.insert(module_directory.to_path_buf(), HashMap::new());
                directories.get_mut(module_directory).unwrap()
            }
        };
        let path_idx = self.module_map.reserve_index();
        //  Give module an anonymous name so it can remain tracked.
        let mut implicitly_named = false;
        let module_name = match module.name {
            Some(ref name) => name.to_string(),
            None => {
                implicitly_named = true;
                let mut anonymous_name = module_path.file_stem()?.to_str()?.to_string();
                while dir_map.get(&anonymous_name).is_some() {
                    anonymous_name.push_str("_x")
                }
                anonymous_name
            }
        };

        if implicitly_named {
            self.errors.push(ProgramError {
                offending_file: path_idx,
                error_type: Importing(errors::nameless_module()),
            })
        }

        let corelib_symbol_idx = self
            .corelib_path
            .map(|path| self.module_map.get(path).unwrap().symbol_idx);
        let typed_module = bind(
            module,
            path_idx,
            &mut self.symbol_table,
            &mut self.errors,
            &mut self.literals,
            corelib_symbol_idx,
        )?;

        // Module names and file names must be equal.
        let file_name = module_path.file_stem()?.to_str()?;
        if !implicitly_named && module_name != file_name {
            self.errors.push(ProgramError {
                offending_file: path_idx,
                error_type: Importing(errors::mismatched_file_and_module_name(
                    &module_name,
                    file_name,
                    module_ident_span,
                )),
            });
        }

        dir_map.insert(module_name, path_idx);
        self.module_map.add(typed_module);

        // Get just added module and resolve imports.
        if self.should_resolve_imports {
            self.resolve_imports_of(path_idx).unwrap();
        }
        Some(path_idx)
    }
    // Resolve the imports of a module given its path index.
    fn resolve_imports_of(&mut self, root_path_idx: PathIndex) -> Result<(), String> {
        // The module requesting imports.
        let base_module = match self.module_map.get(root_path_idx) {
            Some(module) => module,
            None => return Err("Could not get base module".to_owned()),
        };
        let parent_dir = match get_parent_dir(&base_module.path_buf) {
            Some(dir) => dir,
            None => return Err("Could not get parent directory".to_owned()),
        }
        .to_path_buf();

        // Find the modules to import.
        // todo: solve import graphs in parallel.
        // todo: reduce cloning.
        for (target, leaves) in base_module.imports.clone() {
            self.resolve_import_target(&parent_dir, &target, root_path_idx, &leaves)?;
        }
        Ok(())
    }
    /// Resolve an import target.
    fn resolve_import_target(
        &mut self,
        parent_dir: &PathBuf,
        target: &UseTarget,
        root: PathIndex,
        leaves: &Vec<SymbolIndex>,
    ) -> Result<(), String> {
        let path_index = match self.get_or_create_module_in_dir(parent_dir, &target.name.name) {
            Some(path_index) => path_index,
            None => {
                // Unable to resolve the module to a specific path. All is lost.
                self.errors.push(ProgramError {
                    offending_file: root,
                    error_type: Importing(errors::cannot_find_module(
                        target.name.name.clone(),
                        target.name.span,
                    )),
                });
                return Ok(()); // todo: fuzzy search suggestions.
            }
        };
        // Block self referential imports.
        if path_index == root {
            self.errors.push(ProgramError {
                offending_file: root,
                error_type: Importing(errors::self_import(target)),
            });
            return Ok(());
        }
        let imported_module = match self.module_map.get(path_index) {
            Some(imp_module) => imp_module,
            None => return Err("Could not get imported module.".to_owned()),
        };
        let index = imported_module.symbol_idx;
        let solved_import_sources = self.solve_import_symbols(&target, leaves, index, root);
        for (initial, solved) in solved_import_sources {
            let import_symbol = match self.symbol_table.get_mut(initial) {
                Some(symbol) => symbol,
                None => return Err(format!("Tried to get an import index that does not exist.")),
            };
            let mut import_references;
            match &mut import_symbol.kind {
                SemanticSymbolKind::Import { source, .. } => {
                    *source = solved;
                    import_references = import_symbol.references.clone();
                }
                _ => {
                    return Err(format!(
                        "An import symbol is bound incorrectly. Fix binding."
                    ))
                }
            }
            // Join references.
            if let Some(solved) = solved {
                let source_symbol = match self.symbol_table.get_mut(solved) {
                    Some(symbol) => symbol,
                    None => {
                        return Err(format!("Tried to get a solved index that does not exist."))
                    }
                };
                source_symbol.references.append(&mut import_references);
            }
        }
        return Ok(());
    }
    /// Solve import symbols recursively.
    fn solve_import_symbols(
        &mut self,
        target: &UseTarget,
        leaves: &Vec<SymbolIndex>,
        index: SymbolIndex,
        root: PathIndex,
    ) -> Vec<(SymbolIndex, Option<SymbolIndex>)> {
        match &target.path {
            ast::UsePath::Me => vec![(leaves[0], Some(index))],
            ast::UsePath::Item(sub_target) => {
                let imported_symbol_idx =
                    self.look_for_symbol_in_module(index, root, &target, &sub_target);
                if let Some(symbol_idx) = imported_symbol_idx {
                    // Block private imports.
                    let symbol = self.symbol_table.get(symbol_idx).unwrap();
                    if !symbol.kind.is_public() {
                        self.add_import_error_in_file(
                            root,
                            errors::private_symbol_leak(
                                target.name.name.clone(),
                                sub_target.name.to_owned(),
                            ),
                        );
                    }
                    self.solve_import_symbols(&sub_target, leaves, symbol_idx, root)
                } else {
                    self.add_import_error_in_file(
                        root,
                        errors::no_such_symbol_in_module(
                            target.name.name.clone(),
                            sub_target.name.to_owned(),
                        ),
                    );
                    vec![]
                }
            }
            ast::UsePath::List(list) => {
                let mut solved = vec![];
                for (i, sub_target) in list.iter().enumerate() {
                    let leaves = vec![leaves[i]];
                    let imported_symbol_idx =
                        self.look_for_symbol_in_module(index, root, &target, &*sub_target);
                    if let Some(symbol_idx) = imported_symbol_idx {
                        // Block private imports.
                        let symbol = self.symbol_table.get(symbol_idx).unwrap();
                        if !symbol.kind.is_public() {
                            self.add_import_error_in_file(
                                root,
                                errors::private_symbol_leak(
                                    target.name.name.clone(),
                                    sub_target.name.to_owned(),
                                ),
                            );
                        }
                        solved.append(&mut self.solve_import_symbols(
                            &sub_target,
                            &leaves,
                            symbol_idx,
                            root,
                        ))
                    } else {
                        self.add_import_error_in_file(
                            root,
                            errors::no_such_symbol_in_module(
                                target.name.name.clone(),
                                sub_target.name.to_owned(),
                            ),
                        );
                    }
                }
                solved
            }
        }
    }
    /// Handle searching an external module for a symbol.
    fn look_for_symbol_in_module(
        &mut self,
        index: SymbolIndex,
        root: PathIndex,
        target: &UseTarget,
        sub_target: &UseTarget,
    ) -> Option<SymbolIndex> {
        let symbol = self.symbol_table.get(index)?;
        let symbols_in_module = match &symbol.kind {
            SemanticSymbolKind::Module { symbols, .. } => Some(symbols),
            // Imported redirection.
            SemanticSymbolKind::Import { source, .. } => {
                if let Some(index) = source {
                    return self.look_for_symbol_in_module(*index, root, target, sub_target);
                } else {
                    self.errors.push(ProgramError {
                        offending_file: root,
                        error_type: Importing(errors::symbol_not_a_module(target.name.to_owned())),
                    });
                    return None;
                }
            }
            _ => {
                self.errors.push(ProgramError {
                    offending_file: root,
                    error_type: Importing(errors::symbol_not_a_module(target.name.to_owned())),
                });
                return None;
            }
        }?;
        let symbol_to_retrieve = symbols_in_module.iter().find(|symbol| {
            self.symbol_table
                .get(**symbol)
                .is_some_and(|symbol| symbol.name == sub_target.name.name)
        });
        symbol_to_retrieve.copied()
    }
    /// Check if a folder is part of the standpoint.
    pub fn contains_folder(&self, dir: &Path) -> bool {
        self.directories.get(dir).is_some()
    }
    /// Add an import error to the list of errors.
    pub fn add_import_error(&mut self, error: errors::ImportError) {
        self.errors.push(ProgramError {
            offending_file: self.entry_module,
            error_type: Importing(error),
        })
    }
    /// Add an import error to the list of errors.
    pub fn add_import_error_in_file(
        &mut self,
        offending_file: PathIndex,
        error: errors::ImportError,
    ) {
        self.errors.push(ProgramError {
            offending_file,
            error_type: Importing(error),
        })
    }
    /// Check if the standpoint is empty.
    pub fn is_empty(&self) -> bool {
        self.module_map.len() == 0
    }
    /// Check if a file is part of the standpoint.
    pub fn contains_file(&self, path_buf: &PathBuf) -> bool {
        self.module_map
            .paths()
            .find(|module| module.1.path_buf == *path_buf)
            .is_some()
    }
    /// Get the module in a directory using its module name.
    /// The module is freshly analyzed if it exists but it has not been added to the standpoint yet.
    pub fn get_or_create_module_in_dir(&mut self, dir: &Path, name: &str) -> Option<PathIndex> {
        let dir_map = self.directories.get(dir)?;
        if name == "Package" {
            return Some(self.entry_module);
        } else if name == "Super" {
            todo!()
        } else if name == "Core" && self.corelib_path.is_some() {
            return self.corelib_path;
        }
        match dir_map.get(name) {
            // Retrieved successfully.
            Some(path_index) => return Some(*path_index),
            None => {
                // Path either doesn't exist, or has not been added to the standpoint yet.
                // Traverse through the entire folder to determine the path.
                self.create_module_in_dir(dir, name);
                // No possible path to this module was resolved,
                return None;
            }
        }
    }
    /// Rescursively search for and add a module in a directory to the module map.
    fn create_module_in_dir(&mut self, dir: &Path, name: &str) -> Option<PathIndex> {
        for entry_result in dir.read_dir().ok()? {
            let entry = match entry_result {
                Ok(entry) => entry,
                Err(err) => {
                    self.add_import_error(errors::error_reading_entry_file(err));
                    continue;
                }
            };
            let entry_path = entry.path();
            // Path points to a file, easy peasy (peezy?).
            let module_is_file = entry_path.is_file()
                && entry_path.file_stem()?.to_str()? == name
                && entry_path.extension()?.to_str()? == "wrl";
            if module_is_file {
                match Module::from_path(entry_path) {
                    Ok(module) => return self.add_module(module),
                    Err(error) => {
                        self.add_import_error(error);
                        continue;
                    }
                }
            }
            let module_is_directory =
                entry_path.is_dir() && entry_path.file_name()?.to_str()? == name;
            if module_is_directory {
                return self.create_module_in_dir(&entry_path, name);
            }
        }
        return None;
    }
    /// Returns reference to a (typed) module at a specific path, if it exists.
    pub fn get_module_at_path(&self, path_buf: &PathBuf) -> Option<&TypedModule> {
        self.module_map
            .paths()
            .find(|tuple| tuple.1.path_buf == *path_buf)
            .map(|tuple| tuple.1)
    }
    /// Return mutable reference to a (typed) module at a specific path, if it exists.
    pub fn get_mut_module_at_path(&mut self, path_buf: &PathBuf) -> Option<&mut TypedModule> {
        self.module_map
            .paths_mut()
            .find(|tuple| tuple.1.path_buf == *path_buf)
            .map(|tuple| tuple.1)
    }
    /// Changes the content of a single module and updates the entire standpoint accordingly.
    pub fn refresh_module_with_text(&mut self, path: &PathBuf, text: String) -> Option<()> {
        // todo: find more performant ways to implement this.
        let path_idx = self.module_map.map_path_to_index(path)?;
        let module_symbol_idx = self.module_map.get(path_idx)?.symbol_idx;
        let module_symbol = self.symbol_table.remove(module_symbol_idx)?;

        let module_name = module_symbol.name;
        // Get all symbols that were declared in the module to refresh.
        // todo: performanceee.
        let affected_symbols = self
            .symbol_table
            .symbols()
            .filter(|module| {
                module
                    .1
                    .references
                    .first()
                    .is_some_and(|reff| reff.module_path == path_idx)
            })
            .map(|(idx, _)| idx)
            .collect::<Vec<_>>();
        // Get all the connected modules, so they can also be updated.
        for symbol_idx in affected_symbols {
            // Delete the stale affected symbols.
            self.symbol_table.remove(symbol_idx)?;
        }
        // Delete stale semantic errors, and errors related to this module.
        self.errors.retain(|error| {
            !(matches!(
                error.error_type,
                ProgramErrorType::Importing(_) | ProgramErrorType::Typing(_)
            ) || error.offending_file == path_idx)
        });

        // Remove the stale module.
        let stale_module = self.module_map.remove(path_idx)?;
        let parent_directory = get_parent_dir(&stale_module.path_buf)?;
        let dir_map = self.directories.get_mut(parent_directory)?;
        dir_map.remove(&module_name);
        // if it was the Core library that was updated:
        let was_core = self
            .corelib_path
            .is_some_and(|core_path| core_path == path_idx);
        if was_core {
            self.corelib_path = None;
        }

        // Add updated module.
        let mut update = Module::from_text(text);
        update.module_path = Some(path.clone());
        let new_path_idx = self.add_module(update)?;
        if was_core {
            self.corelib_path = Some(new_path_idx);
        }

        // Update all modules.
        // todo: there should be a better way to do this.
        if self.should_resolve_imports {
            let all_paths = self
                .module_map
                .paths()
                .map(|(idx, _)| idx)
                .collect::<Vec<_>>();
            for idx in all_paths {
                self.resolve_imports_of(idx).unwrap();
            }
        }

        Some(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    // todo: use virtual fs.
    use crate::{Module, PathIndex, Standpoint};

    #[test]
    fn bind_variables_and_constants() {
        let text = "
            module Test; 

            public function Main() {
                greeting := \"Say Hello\";
                const CONSTANT: Number = 9090;
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        assert!(standpoint.errors.len() == 1);
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "greeting")
            .is_some());
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "CONSTANT")
            .is_some());
        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            standpoint
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn bind_call_expression() {
        let text = "
            module Test;

            public function Main() {
                greeting := \"Say Hello\";
                Println(greeting);
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        assert!(standpoint.errors.len() == 1);
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "Println")
            .is_some());
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "greeting")
            .is_some());
    }

    #[test]
    fn bind_models() {
        let text = "
            module Test;

            public model Car {
                var make: String,
                var year: UnsignedInt,
                public function Honk() {

                }
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "Car")
            .is_some());
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "Honk")
            .is_some());
    }

    #[test]
    fn bind_this() {
        let text = "
            module Test;

            public model Unit {
                public function Clone(): This {
                    return this;
                }
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        assert!(standpoint.errors.len() == 0);
    }

    #[test]
    fn test_enum_type() {
        let text = "
            module Test;

            public enum Color {
                Red,
                Orange(Color),
                Green
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            standpoint
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        );
        assert!(standpoint.errors.len() == 0);
    }

    #[test]
    fn test_fn_expr() {
        let text = "
            module Test;

            function Main() {
                square := fn(a) a * 2;
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            standpoint
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        );
        assert!(standpoint.errors.len() == 0);
    }

    #[test]
    fn test_use_import() {
        let text = "
            module Test;

            use Core.Io.Println;

            function Main() {
                Println(\"Hello, world!\");
            }
        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "Println")
            .is_some());
        assert!(standpoint
            .symbol_table
            .find(|symbol| symbol.name == "Main")
            .is_some());
        assert!(standpoint.errors.len() == 0);
        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_function() {
        let text = "
        module Main;

function Main() {
}

/// Adds two numbers together.
function Add(a: Int, b: Int): Int {
    return a + b;
}

        ";

        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, true).unwrap();
        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .in_module(PathIndex(0))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn show_imports() {
        let text = "
        module Test;

        use Utils.Sum;

        public function Main() {
            return Sum(1, 2);
        }
        ";

        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, true).unwrap();
        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .in_module(PathIndex(0))
                .collect::<Vec<_>>()
        );
        println!("{:#?}", standpoint.errors);
    }

    #[test]
    fn resolve_single_module_imports() {
        let module0_text = "
    module Main;

    use Test.Utils.Add;

    
        ";
        let module1_text = "
    module Utils;

    type Int = Int;

    public function Add(a: Int, b: Int): Int {
        return a + b;
    }
    ";

        let module2_text = "
    module Test;

    public use Utils;
    public use Utils.Add;

    public function Main() {
        return Add(1, 2);
    }
    ";

        let mut main_module = Module::from_text(format!("{module0_text}"));
        main_module.module_path = Some(PathBuf::from("testing://Main.wrl"));
        let mut utils_module = Module::from_text(format!("{module1_text}"));
        utils_module.module_path = Some(PathBuf::from("testing://Utils.wrl"));
        let mut test_module = Module::from_text(format!("{module2_text}"));
        test_module.module_path = Some(PathBuf::from("testing://Test.wrl"));

        let mut standpoint = Standpoint::build_from_module(utils_module, false).unwrap();
        standpoint.should_resolve_imports = true;
        standpoint.add_module(test_module);
        standpoint.add_module(main_module);

        println!(
            "{:#?}",
            standpoint
                .symbol_table
                .symbols()
                .map(|(_, symbol)| symbol)
                .collect::<Vec<_>>()
        );
        println!("{:#?}", standpoint.errors);
        assert!(standpoint.errors.len() == 0);
    }

    #[test]
    fn resolve_mutliple_module_imports() {
        let module0_text = "
    module Main;

    use Test.{Utils.Add, Divide};

    
        ";
        let module1_text = "
    module Utils;

    type Int = Int;

    public function Add(a: Int, b: Int): Int {
        return a + b;
    }

    public function Divide(a: Int, b: Int): Int {
        return a / b;
    }
    ";

        let module2_text = "
    module Test;

    public use Utils;
    public use Utils.Add;
    public use Utils.Divide;

    public type Function = fn (a: Function): Function;

    public function Main() {
        return Add(1, 2);
    }
    ";

        let mut main_module = Module::from_text(format!("{module0_text}"));
        main_module.module_path = Some(PathBuf::from("testing://Main.wrl"));
        let mut utils_module = Module::from_text(format!("{module1_text}"));
        utils_module.module_path = Some(PathBuf::from("testing://Utils.wrl"));
        let mut test_module = Module::from_text(format!("{module2_text}"));
        test_module.module_path = Some(PathBuf::from("testing://Test.wrl"));

        let mut standpoint = Standpoint::build_from_module(utils_module, false).unwrap();
        standpoint.should_resolve_imports = true;
        standpoint.add_module(test_module);
        standpoint.add_module(main_module);

        println!(
            "{:?}",
            standpoint
                .symbol_table
                .symbols()
                .map(|(_, symbol)| (&symbol.name, &symbol.references, &symbol.kind))
                .collect::<Vec<_>>()
        );
        println!("{:#?}", standpoint.errors);
        assert!(standpoint.errors.len() == 0);
    }

    #[test]
    fn test_duplication() {
        let text = "
            module Test;

            public type Maybe = Maybe;

            public model Maybe<T> implements Assertable<T> + Try<T> {}

        ";
        let mut module = Module::from_text(format!("{text}"));
        module.module_path = Some(PathBuf::from("testing://Test.wrl"));
        let standpoint = Standpoint::build_from_module(module, false).unwrap();
        println!("{:#?}", standpoint);
    }
}
