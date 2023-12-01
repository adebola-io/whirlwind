#[cfg(test)]
mod tests;

use super::{symbols::*, ProgramError};
use crate::{
    bind, typecheck, CurrentModuleType, IntrinsicPaths, LiteralMap, Module, ModuleMap, PathIndex,
    ProgramErrorType::{self, Importing},
    SymbolTable, TypedModule, BASE_CORE_PATH, PRELUDE_PATH,
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
    pub literals: LiteralMap,
    pub errors: Vec<ProgramError>,
    /// This flag permits the standpoint to update itself automatically
    /// when operations like adding, removing and refreshing modules
    /// occur.
    pub auto_update: bool,
    /// The path to the entry module of the core library.
    pub corelib_path: Option<PathIndex>,
    /// The path to the prelude module of the core library.
    pub prelude_path: Option<PathIndex>,
    // pub warnings: Vec<Warnings>,
}

impl IntrinsicPaths for Standpoint {}

pub enum StandpointStatus {
    RefreshSuccessful,
    Restarted,
}

impl Standpoint {
    /// Creates a new standpoint.
    pub fn new(should_resolve_imports: bool, corelib_path: Option<PathBuf>) -> Self {
        let mut standpoint = Standpoint {
            module_map: ModuleMap::new(),
            entry_module: PathIndex(0),
            directories: HashMap::new(),
            symbol_table: SymbolTable::new(),
            literals: LiteralMap::new(),
            errors: vec![],
            root_folder: None,
            auto_update: should_resolve_imports,
            corelib_path: None,
            prelude_path: None,
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
    /// Check if the standpoint is empty.
    pub fn is_empty(&self) -> bool {
        self.module_map.len() == 0
    }
    /// Get the module in a directory using its module name.
    /// The module is freshly analyzed if it exists but it has not been added to the standpoint yet.
    pub fn get_or_create_module_in_dir(&mut self, dir: &Path, name: &str) -> Option<PathIndex> {
        let (name, dir_map) = if name == "Package" {
            return Some(self.entry_module);
        } else if name == "Super" {
            let parent_folder_of_dir = get_parent_dir(dir)?;
            let dir_map = self.directories.get(parent_folder_of_dir)?;
            (parent_folder_of_dir.file_stem()?.to_str()?, dir_map)
        } else if name == "Core" && self.corelib_path.is_some() {
            return self.corelib_path;
        } else {
            (name, self.directories.get(dir)?)
        };

        match dir_map.get(name) {
            // Retrieved successfully.
            Some(path_index) => return Some(*path_index),
            None => {
                // Path either doesn't exist, or has not been added to the standpoint
                // or might be nested in a named directory.
                // Traverse through the entire folder to determine the path.
                return self.create_module_or_retrieve_nested(dir, name);
            }
        }
    }
    /// Return mutable reference to a (typed) module at a specific path, if it exists.
    pub fn get_mut_module_at_path(&mut self, path_buf: &PathBuf) -> Option<&mut TypedModule> {
        self.module_map
            .paths_mut()
            .find(|tuple| tuple.1.path_buf == *path_buf)
            .map(|tuple| tuple.1)
    }
    /// Adds an error to the error list.
    /// Deduplicates doubled import errors.
    fn add_error(&mut self, error: ProgramError) {
        if matches!(&error.error_type, Importing(_)) {
            if self.errors.iter().any(|prior_error| *prior_error == error) {
                return;
            }
        }
        self.errors.push(error);
    }
    /// Clears the entire standpoint and rebuilds it again.
    pub fn restart(&mut self) -> Option<()> {
        let entry_path = self.module_map.get(self.entry_module)?.path_buf.clone();
        let core_lib_path = self
            .corelib_path
            .and_then(|path| self.module_map.get(path))
            .map(|module| module.path_buf.clone());
        *self = Standpoint::new(self.auto_update, core_lib_path);
        self.entry_module = self.add_module(Module::from_path(entry_path).ok()?)?;
        Some(())
    }
}

// IMPORT OPERATIONS
impl Standpoint {
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
        leaves: &[SymbolIndex],
    ) -> Result<(), String> {
        let path_index = match self.get_or_create_module_in_dir(parent_dir, &target.name.name) {
            Some(path_index) => path_index,
            None => {
                // Unable to resolve the module to a specific path. All is lost.
                self.add_error(ProgramError {
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
            self.add_error(ProgramError {
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
        leaves: &[SymbolIndex],
        index: SymbolIndex,
        root: PathIndex,
    ) -> Vec<(SymbolIndex, Option<SymbolIndex>)> {
        match &target.path {
            // Importing a module itself.
            ast::UsePath::Me => vec![(
                match leaves.get(0) {
                    Some(value) => *value,
                    None => return vec![],
                },
                Some(index),
            )],
            // Importing an item of a module.
            ast::UsePath::Item(sub_target) => {
                let imported_symbol_idx =
                    self.look_for_symbol_in_module(index, root, &target, &sub_target);
                if let Some(symbol_idx) = imported_symbol_idx {
                    // Block private imports.
                    let symbol = self.symbol_table.get(symbol_idx).unwrap();
                    if !symbol.kind.is_public() {
                        self.add_error(ProgramError {
                            offending_file: root,
                            error_type: ProgramErrorType::Importing(errors::private_symbol_leak(
                                target.name.name.clone(),
                                sub_target.name.to_owned(),
                            )),
                        });
                    }
                    self.solve_import_symbols(&sub_target, leaves, symbol_idx, root)
                } else {
                    vec![]
                }
            }
            ast::UsePath::List(_) => {
                // scatter single target into distinct targets and follow each one.
                let all_paths = target.scatter();
                let mut solutions = vec![];
                for (i, target) in all_paths.iter().enumerate() {
                    solutions.append(&mut self.solve_import_symbols(
                        &target,
                        &[match leaves.get(i) {
                            Some(index) => *index,
                            // Could be mismatched if there are values with same name.
                            None => continue,
                        }],
                        index,
                        root,
                    ))
                }
                solutions
            }
        }
    }
    /// Handle searching an external module for a symbol.
    fn look_for_symbol_in_module(
        &mut self,
        imported_idx: SymbolIndex,
        root: PathIndex,
        target: &UseTarget,
        sub_target: &UseTarget,
    ) -> Option<SymbolIndex> {
        let imported_module_symbol = self.symbol_table.get_mut(imported_idx)?;
        imported_module_symbol.add_reference(root, target.name.span);
        // drop mutable reference.
        let imported_module_symbol = self.symbol_table.get(imported_idx)?;
        let symbols_in_imported_module = match &imported_module_symbol.kind {
            SemanticSymbolKind::Module {
                global_declaration_symbols: symbols,
                ..
            } => Some(symbols),
            // Imported redirection.
            SemanticSymbolKind::Import { source, .. } => {
                if source.is_some() {
                    let mut source = source.clone();
                    while source.is_some() {
                        let imported_symbol = self.symbol_table.get(source.clone().unwrap());
                        match imported_symbol {
                            Some(SemanticSymbol {
                                kind:
                                    SemanticSymbolKind::Import {
                                        source: parent_source,
                                        ..
                                    },
                                ..
                            }) => source = *parent_source,
                            _ => break,
                        }
                    }
                    return self.look_for_symbol_in_module(
                        source.unwrap(),
                        root,
                        target,
                        sub_target,
                    );
                } else {
                    // Errors have to be checked to ensure they are not duplicated.
                    // Duplicated can happen when list targets are scattered, and each distinct target encounters the same
                    // error.
                    let error = ProgramError {
                        offending_file: root,
                        error_type: Importing(errors::symbol_not_a_module(target.name.to_owned())),
                    };
                    self.add_error(error);
                    return None;
                }
            }
            _ => {
                let error = ProgramError {
                    offending_file: root,
                    error_type: Importing(errors::symbol_not_a_module(target.name.to_owned())),
                };
                self.add_error(error);
                return None;
            }
        }?;
        let symbol_to_retrieve = symbols_in_imported_module.iter().find(|symbol| {
            self.symbol_table
                .get(**symbol)
                .is_some_and(|symbol| symbol.name == sub_target.name.name)
        });
        let symbol_to_retrieve = symbol_to_retrieve.copied();
        if symbol_to_retrieve.is_none() {
            self.add_import_error_in_file(
                root,
                errors::no_such_symbol_in_module(
                    target.name.name.clone(),
                    sub_target.name.to_owned(),
                ),
            );
        };
        symbol_to_retrieve
    }
    /// Add an import error to the list of errors.
    pub fn add_import_error(&mut self, error: errors::ImportError) {
        self.add_error(ProgramError {
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
        self.add_error(ProgramError {
            offending_file,
            error_type: Importing(error),
        })
    }
}

// MODULE OPERATIONS
impl Standpoint {
    /// Add a regular module to the standpoint and returns its path index.
    pub fn add_module(&mut self, module: Module) -> Option<PathIndex> {
        let module_path = module.module_path.as_ref()?.to_owned();
        let module_file_name = module_path.file_stem()?.to_str()?;
        let module_directory = get_parent_dir(&module_path)?;

        let directories = &mut self.directories;
        let module_ident_span = module
            .ambience
            .module_name
            .as_ref()
            .map(|i| i.span)
            .unwrap_or_default();
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
        let (module_name, implicitly_named) = match module.name {
            Some(ref name) => (name.to_string(), false),
            None => {
                let mut anonymous_name = module_file_name.to_string();
                while dir_map.get(&anonymous_name).is_some() {
                    anonymous_name.push_str("_")
                }
                (anonymous_name, true)
            }
        };
        if implicitly_named {
            // todo: find out why using self.add_error here is illegal.
            self.errors.push(ProgramError {
                offending_file: path_idx,
                error_type: Importing(errors::nameless_module()),
            })
        }

        let corelib_symbol_idx = self
            .corelib_path
            .and_then(|path| self.module_map.get(path))
            .map(|t_module| t_module.symbol_idx);
        let prelude_symbol_idx = self
            .prelude_path
            .and_then(|path| self.module_map.get(path))
            .map(|t_module| t_module.symbol_idx);

        // intrinsic paths.
        let current_module_type = if let Ok(path) = module_path.strip_prefix(BASE_CORE_PATH) {
            if let Some(path_str) = path.to_str() {
                match path_str {
                    Self::ARRAY => CurrentModuleType::Array,
                    Self::ASYNC => CurrentModuleType::Async,
                    Self::BOOL => CurrentModuleType::Bool,
                    Self::NUMERIC => CurrentModuleType::Numeric,
                    Self::INTERNAL => CurrentModuleType::Internal,
                    Self::ITERATABLE => CurrentModuleType::Iteratable,
                    Self::OPS => CurrentModuleType::Ops,
                    Self::TRAITS => CurrentModuleType::Traits,
                    Self::RANGE => CurrentModuleType::Range,
                    Self::STRING => CurrentModuleType::String,
                    Self::DEFAULT => CurrentModuleType::Default,
                    Self::MAYBE => CurrentModuleType::Maybe,
                    _ => CurrentModuleType::Regular,
                }
            } else {
                CurrentModuleType::Regular
            }
        } else {
            CurrentModuleType::Regular
        };

        let typed_module = bind(
            module_name.clone(),
            module,
            path_idx,
            &mut self.symbol_table,
            &mut self.errors,
            &mut self.literals,
            corelib_symbol_idx,
            prelude_symbol_idx,
            current_module_type,
        )?;

        // Module names and file names must be equal.
        if !implicitly_named && module_name != module_file_name {
            self.errors.push(ProgramError {
                offending_file: path_idx,
                error_type: Importing(errors::mismatched_file_and_module_name(
                    &module_name,
                    module_file_name,
                    module_ident_span,
                )),
            });
        }

        dir_map.insert(module_name, path_idx);
        self.module_map.add(typed_module);

        // Get just added module and resolve imports.
        if self.auto_update {
            self.resolve_imports_of(path_idx).unwrap();
        }

        // Mark the prelude module.
        if module_path.as_os_str().to_str()? == PRELUDE_PATH {
            self.prelude_path = Some(path_idx);
        }

        Some(path_idx)
    }

    /// Removes the module with a particlar index and all its related characteristics.
    pub fn remove_module(&mut self, path_idx: PathIndex) -> Option<TypedModule> {
        let module_symbol_idx = self.module_map.get(path_idx)?.symbol_idx;
        let module_symbol = self.symbol_table.remove(module_symbol_idx)?;
        let module_name = module_symbol.name;
        let literals_to_remove = self
            .literals
            .literals()
            .filter_map(|(idx, literal)| {
                if match literal {
                    Literal::StringLiteral { module, .. }
                    | Literal::NumericLiteral { module, .. }
                    | Literal::BooleanLiteral { module, .. } => *module == path_idx,
                } {
                    return Some(idx);
                }
                return None;
            })
            .collect::<Vec<_>>();
        let mut symbols_to_prune = vec![];
        let symbols_to_remove = self
            .symbol_table
            .symbols()
            .filter_map(|(idx, symbol)| {
                // Symbol is declared in this module.
                if symbol
                    .references
                    .first()
                    .is_some_and(|first| first.module_path == path_idx)
                {
                    return Some(idx);
                }
                // Symbol is referenced in the symbol.
                if symbol
                    .references
                    .iter()
                    .skip(1)
                    .find(|reference| reference.module_path == path_idx)
                    .is_some()
                {
                    symbols_to_prune.push(idx);
                    return None;
                }
                return None;
            })
            .collect::<Vec<_>>();
        for symbol_idx in symbols_to_remove {
            self.symbol_table.remove(symbol_idx);
        }
        for literal_idx in literals_to_remove {
            self.literals.remove(literal_idx);
        }
        for symbol_idx in symbols_to_prune {
            if let Some(symbol) = self.symbol_table.get_mut(symbol_idx) {
                symbol
                    .references
                    .retain(|reference| reference.module_path != path_idx);
            }
        }
        // delete stale errors.
        self.errors.retain(|error| {
            !(matches!(
                error.error_type,
                ProgramErrorType::Importing(_) | ProgramErrorType::Typing(_)
            ) || error.offending_file == path_idx)
        });
        let stale_module = self.module_map.remove(path_idx)?;
        let parent_directory = get_parent_dir(&stale_module.path_buf)?;
        let dir_map = self.directories.get_mut(parent_directory)?;
        dir_map.remove(&module_name);
        if dir_map.is_empty() {
            self.directories.remove(parent_directory);
        }
        // Update all modules.
        // todo: there should be a better way to do this.
        if self.auto_update {
            self.refresh_imports();
        }
        Some(stale_module)
    }

    /// Refreshes all import resolutions in the entire standpoint.
    pub fn refresh_imports(&mut self) {
        let all_paths = self
            .module_map
            .paths()
            .map(|(idx, _)| idx)
            .collect::<Vec<_>>();
        for idx in all_paths {
            // Remove all stale errors.
            self.errors.retain(|error| {
                !(error.offending_file == idx
                    && (matches!(error.error_type, ProgramErrorType::Importing(_))))
            });
            self.resolve_imports_of(idx).unwrap();
        }
    }

    /// Changes the content of a single module and updates the entire standpoint accordingly.
    pub fn refresh_module(
        &mut self,
        path_idx: PathIndex,
        text: &String,
        should_recompute_imports: bool,
    ) -> Option<StandpointStatus> {
        let path = self.module_map.get(path_idx)?.path_buf.clone();
        // // Disabling auto update prevents unecessary import solving.
        self.auto_update = false;
        std::mem::drop(self.remove_module(path_idx)?);
        self.auto_update = true;

        // Add updated module.
        let mut update = Module::from_text(&text);
        update.module_path = Some(path.clone());
        let new_path_idx = self.add_module(update)?;
        // if it was the Core library that was updated:
        if self
            .corelib_path
            .is_some_and(|core_path| core_path == path_idx)
        {
            self.corelib_path = Some(new_path_idx);
        }

        // // todo: find more performant ways to implement this.
        // Update all modules.
        if should_recompute_imports && self.auto_update {
            self.refresh_imports();
            self.check_all_modules();
        } else {
            self.check_module(path_idx);
        }
        Some(StandpointStatus::RefreshSuccessful)
    }

    /// Recursively search for and add a module in a directory to the module map.
    pub fn create_module_or_retrieve_nested(
        &mut self,
        dir: &Path,
        module_name: &str,
    ) -> Option<PathIndex> {
        for entry_result in dir.read_dir().ok()? {
            let entry = match entry_result {
                Ok(entry) => entry,
                Err(_) => continue,
            };
            let entry_path = entry.path();
            // Path points to a file, easy peasy (peezy?).
            let module_is_file = entry_path.is_file()
                && entry_path.file_stem()?.to_str()? == module_name
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
                entry_path.is_dir() && entry_path.file_name()?.to_str()? == module_name;
            if module_is_directory {
                // Check if this is a named directory that is already being tracked.
                if let Some(dir_map) = self.directories.get(&entry_path) {
                    if let Some(index) = dir_map.get(module_name) {
                        return Some(*index);
                    }
                }
                return self.create_module_or_retrieve_nested(&entry_path, module_name);
            }
        }
        // No possible path to this module was resolved,
        return None;
    }
}

// DIRECTORY OPERATIONS
impl Standpoint {
    /// Check if a folder is part of the standpoint.
    pub fn contains_folder(&self, dir: &Path) -> bool {
        self.directories.get(dir).is_some()
    }

    /// Check if a file is part of the standpoint.
    pub fn contains_file(&self, path_buf: &PathBuf) -> bool {
        self.module_map
            .paths()
            .find(|module| module.1.path_buf == *path_buf)
            .is_some()
    }

    /// Returns reference to a (typed) module at a specific path, if it exists.
    pub fn get_module_at_path(&self, path_buf: &PathBuf) -> Option<&TypedModule> {
        self.module_map
            .paths()
            .find(|tuple| tuple.1.path_buf == *path_buf)
            .map(|tuple| tuple.1)
    }
}

// TYPE OPERATIONS
impl Standpoint {
    /// Runs the typechecker on a module.
    pub fn check_module(&mut self, module_path_idx: PathIndex) -> Option<()> {
        let module = self.module_map.get_mut(module_path_idx)?;
        typecheck(
            module,
            &mut self.symbol_table,
            &mut self.errors,
            &self.literals,
        );
        Some(())
    }

    /// Typecheck all modules in the standpoint.
    pub fn check_all_modules(&mut self) {
        let all_paths = self
            .module_map
            .paths()
            .map(|(idx, _)| idx)
            .collect::<Vec<_>>();
        for idx in all_paths {
            self.check_module(idx).unwrap();
        }
    }
}
