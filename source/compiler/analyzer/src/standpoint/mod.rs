#[cfg(test)]
mod tests;

use super::{symbols::*, ProgramDiagnostic};
use crate::{
    bind, typecheck, CurrentModuleType, DiagnosticType,
    Error::{self, Importing},
    IntrinsicPaths, LiteralMap, Module, ModuleMap, PathIndex, SurfaceAreaCalculator, SymbolLibrary,
    SymbolTable, TypedFunctionDeclaration, TypedModule, TypedStmnt,
};
use ast::UseTarget;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use utils::get_parent_dir;

/// The standpoint is the final and complete intermediate representation
/// in the compiler frontend. It consists of every module in the program,
/// and is responsible for tasks like resolving imports, creating and managing
/// symbol tables, typechecking, binding declarations, static size analysis etc.
#[derive(Debug)]
pub struct Standpoint {
    pub module_map: ModuleMap,
    pub root_folder: Option<PathBuf>,
    pub entry_module: PathIndex,
    pub directories: HashMap<PathBuf, HashMap<String, PathIndex>>,
    /// All symbols in the program, divided by the modules in which
    /// they are declared.
    pub symbol_library: SymbolLibrary,
    /// All strings, numbers and booleans in the program.
    pub literals: LiteralMap,
    /// Typing, importing, binding, parsing and lexing errors
    /// and warnings produced throughout the analyzing of the program.
    pub diagnostics: Vec<ProgramDiagnostic>,
    /// This flag permits the standpoint to update itself automatically
    /// when operations like adding, removing and refreshing modules
    /// occur.
    pub auto_update: bool,
    /// The path to the entry module of the core library.
    pub corelib_path: Option<PathIndex>,
    /// The path to the prelude module of the core library.
    pub prelude_path: Option<PathIndex>,
}

impl IntrinsicPaths for Standpoint {}

impl Standpoint {
    /// Creates a new standpoint.
    pub fn new(should_resolve_imports: bool, corelib_path: Option<PathBuf>) -> Self {
        let mut standpoint = Standpoint {
            module_map: ModuleMap::new(),
            entry_module: PathIndex(0),
            directories: HashMap::new(),
            symbol_library: SymbolLibrary::new(),
            literals: LiteralMap::new(),
            diagnostics: vec![],
            root_folder: None,
            auto_update: should_resolve_imports,
            corelib_path: None,
            prelude_path: None,
        };
        //todo: if corelib is already loaded in another standpoint.
        if let Some(path) = corelib_path {
            match Module::from_path(path) {
                Ok(module) => {
                    standpoint.auto_update = false;
                    if let Some(idx) = standpoint.add_module(module) {
                        standpoint.auto_update = should_resolve_imports;
                        standpoint.corelib_path = Some(idx);
                        if standpoint.auto_update {
                            standpoint.analyze_imports(idx, ResolutionPhase::Discovery);
                        }
                    }
                }
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
            self.symbol_library
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
    pub fn get_or_create_module_in_dir(
        &mut self,
        dir: &Path,
        name: &str,
        phase: ResolutionPhase,
    ) -> Option<PathIndex> {
        let (name, dir_map) = if name == "package" {
            return Some(self.entry_module);
        } else if name == "super" {
            let parent_folder_of_dir = get_parent_dir(dir)?;
            let dir_map = self.directories.get(parent_folder_of_dir)?;
            (parent_folder_of_dir.file_stem()?.to_str()?, dir_map)
        } else if name == "core" && self.corelib_path.is_some() {
            return self.corelib_path;
        } else {
            // todo: packages. namespace.
            (name, self.directories.get(dir)?)
        };

        match dir_map.get(name) {
            // Retrieved successfully.
            Some(path_index) => return Some(*path_index),
            None => {
                // Path either doesn't exist, or has not been added to the standpoint
                // or might be nested in a named directory.
                // Traverse through the entire folder to determine the path.
                return self.create_module_or_retrieve_nested(dir, name, phase);
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
    fn add_diagnostic(&mut self, error: ProgramDiagnostic) {
        if matches!(&error._type, DiagnosticType::Error(Importing(_))) {
            if self
                .diagnostics
                .iter()
                .any(|prior_error| *prior_error == error)
            {
                return;
            }
        }
        self.diagnostics.push(error);
    }
    /// The validate method is responsible for assessing all imports in the standpoint
    /// and building type information for every module in the program.
    pub fn validate(&mut self) {
        self.diagnostics.retain(|error| {
            !matches!(
                error._type,
                DiagnosticType::Error(Error::Typing(_) | Error::Importing(_))
                    | DiagnosticType::Warning(_)
            )
        });
        self.symbol_library.type_environments.clear();
        self.resolve_imports();
        self.check_all_modules();
    }

    pub fn restart_and_exclude(&mut self, path_idx: PathIndex) {
        let modules = self
            .module_map
            .paths()
            .filter(|(idx, _)| *idx != path_idx)
            .map(|(_, typed_module)| typed_module.path_buf.clone())
            .collect::<Vec<_>>();
        let core_lib_path = self
            .corelib_path
            .and_then(|idx| self.module_map.get(idx))
            .map(|module| module.path_buf.clone());
        *self = Standpoint::new(true, core_lib_path);
        for module_path in modules {
            if self.module_map.map_path_to_index(&module_path).is_none() {
                match Module::from_path(module_path) {
                    Ok(module) => {
                        self.add_module(module);
                    }
                    Err(_) => continue,
                }
            }
        }
    }
    /// Returns the entry function in the main module.
    pub fn main(&self) -> Option<&TypedFunctionDeclaration> {
        let entry_module = self.module_map.get(self.entry_module)?;
        for statement in &entry_module.statements {
            if let TypedStmnt::FunctionDeclaration(f) = statement {
                let function_symbol = ast::unwrap_or_continue!(self.symbol_library.get(f.name));
                if function_symbol.name == "main" {
                    return Some(f);
                }
            }
        }
        return None;
    }
    /// Returns the symbollib, literal and module maps.
    pub fn data(&self) -> (&LiteralMap, &SymbolLibrary, &ModuleMap) {
        (&self.literals, &self.symbol_library, &self.module_map)
    }
}

/// The point of having two separate phases for resolution
/// is to deal with cyclic use statements.
#[derive(Clone, Copy)]
pub enum ResolutionPhase {
    /// A half check for imports. It does not produce any errors.
    Discovery,
    /// It should happen after discovery. It checks that all the imports
    /// expected have been added, and produces errors otherwise.
    Assessment,
}

impl ResolutionPhase {
    /// Returns `true` if the resolution phase is [`Assessment`].
    pub fn is_assessment(&self) -> bool {
        matches!(self, Self::Assessment)
    }
}

// IMPORT OPERATIONS
impl Standpoint {
    // Resolve the imports of a module given its path index.
    pub fn analyze_imports(&self, root_path_idx: PathIndex, phase: ResolutionPhase) {
        // The module requesting imports.
        let base_module = match self.module_map.get(root_path_idx) {
            Some(module) => module,
            None => return,
        };
        let parent_dir = match get_parent_dir(&base_module.path_buf) {
            Some(dir) => dir,
            None => return,
        }
        .to_path_buf();
        // Please ignore the next two lines. They are symptoms of a skill issue.
        #[allow(mutable_transmutes)]
        let this = unsafe { std::mem::transmute::<&Standpoint, &mut Standpoint>(self) };
        // Find the modules to import.
        // todo: solve import graphs in parallel.
        // todo: reduce cloning.
        for (target, leaves) in &base_module.imports {
            this.resolve_import_target(&parent_dir, target, root_path_idx, leaves, phase);
        }
    }

    /// Resolve an import target.
    fn resolve_import_target(
        &mut self,
        parent_dir: &PathBuf,
        target: &UseTarget,
        root: PathIndex,
        leaves: &[SymbolIndex],
        phase: ResolutionPhase,
    ) {
        let path_index =
            match self.get_or_create_module_in_dir(parent_dir, &target.name.name, phase) {
                Some(path_index) => path_index,
                None => {
                    if phase.is_assessment() {
                        // Unable to resolve the module to a specific path. All is lost.
                        self.add_diagnostic(ProgramDiagnostic {
                            offending_file: root,
                            _type: DiagnosticType::Error(Importing(errors::cannot_find_module(
                                target.name.name.clone(),
                                target.name.span,
                            ))),
                        });
                    }
                    return; // todo: fuzzy search suggestions.
                }
            };
        // Block self referential imports.
        if path_index == root {
            if phase.is_assessment() {
                self.add_diagnostic(ProgramDiagnostic {
                    offending_file: root,
                    _type: DiagnosticType::Error(Importing(errors::self_import(target))),
                });
            }
            return;
        }
        let imported_module = match self.module_map.get(path_index) {
            Some(imp_module) => imp_module,
            None => return,
        };
        let index = imported_module.symbol_idx;
        let solved_import_sources = self.solve_import_symbols(&target, leaves, index, root, phase);
        for (initial, solved) in solved_import_sources {
            let import_symbol = match self.symbol_library.get_mut(initial) {
                Some(symbol) => symbol,
                None => return,
            };
            let mut import_references;
            match &mut import_symbol.kind {
                SemanticSymbolKind::Import { source, .. } => {
                    *source = solved;
                    import_references = import_symbol.references.clone();
                }
                _ => {
                    return;
                }
            }
            // Join references.
            if let Some(solved) = solved {
                let source_symbol = match self.symbol_library.get_mut(solved) {
                    Some(symbol) => symbol,
                    None => return,
                };
                source_symbol.references.append(&mut import_references);
            }
        }
    }

    /// Solve import symbols recursively.
    fn solve_import_symbols(
        &mut self,
        target: &UseTarget,
        leaves: &[SymbolIndex],
        index: SymbolIndex,
        root: PathIndex,
        phase: ResolutionPhase,
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
                    self.look_for_symbol_in_module(index, root, &target, &sub_target, phase);
                if let Some(symbol_idx) = imported_symbol_idx {
                    // Block private imports.
                    let symbol = self.symbol_library.get(symbol_idx).unwrap();
                    if phase.is_assessment() {
                        if !symbol.kind.is_public() {
                            self.add_diagnostic(ProgramDiagnostic {
                                offending_file: root,
                                _type: DiagnosticType::Error(Error::Importing(
                                    errors::private_symbol_leak(
                                        target.name.name.clone(),
                                        sub_target.name.to_owned(),
                                    ),
                                )),
                            });
                        }
                    }
                    self.solve_import_symbols(&sub_target, leaves, symbol_idx, root, phase)
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
                        phase,
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
        phase: ResolutionPhase,
    ) -> Option<SymbolIndex> {
        let imported_module_symbol = self.symbol_library.get_mut(imported_idx)?;
        imported_module_symbol.add_reference(root, target.name.span);
        // drop mutable reference.
        let imported_module_symbol = self.symbol_library.get(imported_idx)?;
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
                        let imported_symbol = self.symbol_library.get(source.clone().unwrap());
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
                        phase,
                    );
                } else {
                    if phase.is_assessment() {
                        // Errors have to be checked to ensure they are not duplicated.
                        // Duplicated can happen when list targets are scattered, and each distinct target encounters the same
                        // error.
                        let error = ProgramDiagnostic {
                            offending_file: root,
                            _type: DiagnosticType::Error(Importing(errors::symbol_not_a_module(
                                target.name.to_owned(),
                            ))),
                        };
                        self.add_diagnostic(error);
                    }
                    return None;
                }
            }
            _ => {
                if phase.is_assessment() {
                    let error = ProgramDiagnostic {
                        offending_file: root,
                        _type: DiagnosticType::Error(Importing(errors::symbol_not_a_module(
                            target.name.to_owned(),
                        ))),
                    };
                    self.add_diagnostic(error);
                }
                return None;
            }
        }?;
        let symbol_to_retrieve = symbols_in_imported_module.iter().find(|symbol| {
            self.symbol_library
                .get(**symbol)
                .is_some_and(|symbol| symbol.name == sub_target.name.name)
        });
        let symbol_to_retrieve = symbol_to_retrieve.copied();
        if symbol_to_retrieve.is_none() && phase.is_assessment() {
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
        self.add_diagnostic(ProgramDiagnostic {
            offending_file: self.entry_module,
            _type: DiagnosticType::Error(Importing(error)),
        })
    }

    /// Add an import error to the list of errors.
    pub fn add_import_error_in_file(
        &mut self,
        offending_file: PathIndex,
        error: errors::ImportError,
    ) {
        self.add_diagnostic(ProgramDiagnostic {
            offending_file,
            _type: DiagnosticType::Error(Importing(error)),
        })
    }

    /// Resolves all imports in the standpoint, and produces errors
    /// for unresolvable items.
    pub fn resolve_imports(&mut self) {
        for (idx, _) in self.module_map.paths() {
            self.analyze_imports(idx, ResolutionPhase::Assessment);
        }
    }
}

// MODULE OPERATIONS
impl Standpoint {
    /// Add a regular module to the standpoint and returns its path index.
    pub fn add_module(&mut self, module: Module) -> Option<PathIndex> {
        let module_path = module.module_path.as_ref()?.to_owned();
        let module_file_name = module_path.file_stem()?.to_str()?;
        let module_directory = get_parent_dir(&module_path)?;
        let mut is_prelude = false;

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
            let error = ProgramDiagnostic {
                offending_file: path_idx,
                _type: DiagnosticType::Error(Error::Context(errors::nameless_module())),
            };
            if !self.diagnostics.iter().any(|prior| *prior == error) {
                self.diagnostics.push(error)
            }
        }

        let corelib_symbol_idx = self
            .corelib_path
            .and_then(|path| self.module_map.get(path))
            .map(|t_module| t_module.symbol_idx);
        let prelude_symbol_idx = self
            .prelude_path
            .and_then(|path| self.module_map.get(path))
            .map(|t_module| t_module.symbol_idx);

        let core_path_folder = self
            .corelib_path
            .and_then(|core_path_idx| self.module_map.get(core_path_idx))
            .and_then(|module| get_parent_dir(&module.path_buf))
            .and_then(|folder| folder.to_str());

        let mut allow_global = true;
        // intrinsic paths.
        let current_module_type = if let Some(folder) = core_path_folder {
            if let Ok(path) = module_path.strip_prefix(folder) {
                // To prevent unnecessary import errors, global prelude symbols are not
                // allowed to be accessed anywhere else inside the code of the core
                // library.
                allow_global = false;
                let path_str = path.to_string_lossy().replace("\\", "/");
                match path_str.as_str() {
                    Self::ARRAY => CurrentModuleType::Array,
                    Self::CONCURRENT => CurrentModuleType::Concurrent,
                    Self::BOOL => CurrentModuleType::Boolean,
                    Self::NUMERIC => CurrentModuleType::Numeric,
                    Self::INTERNAL => CurrentModuleType::Internal,
                    Self::ITERABLE => CurrentModuleType::Iteration,
                    Self::OPS => CurrentModuleType::Ops,
                    Self::INTERFACES => CurrentModuleType::Interfaces,
                    Self::RANGE => CurrentModuleType::Range,
                    Self::STRING => CurrentModuleType::String,
                    Self::DEFAULT => CurrentModuleType::Default,
                    Self::TRY => CurrentModuleType::Try,
                    Self::GUARANTEED => CurrentModuleType::Guaranteed,
                    Self::MAYBE => CurrentModuleType::Maybe,
                    Self::NEVER => CurrentModuleType::Never,
                    Self::PRELUDE => {
                        is_prelude = true;
                        CurrentModuleType::Regular
                    }
                    Self::PROSPECT => CurrentModuleType::Prospect,
                    Self::EVENTUAL => CurrentModuleType::Eventual,
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
            &mut self.symbol_library,
            &mut self.diagnostics,
            &mut self.literals,
            corelib_symbol_idx,
            prelude_symbol_idx,
            current_module_type,
            allow_global,
        )?;

        // Module names and file names must be equal.
        if !implicitly_named && module_name != module_file_name {
            self.diagnostics.push(ProgramDiagnostic {
                offending_file: path_idx,
                _type: DiagnosticType::Error(Error::Context(
                    errors::mismatched_file_and_module_name(
                        &module_name,
                        module_file_name,
                        module_ident_span,
                    ),
                )),
            });
        }

        dir_map.insert(module_name, path_idx);
        self.module_map.add(typed_module);

        // Mark the prelude module.
        if is_prelude {
            self.prelude_path = Some(path_idx);
        }

        // Get just added module and resolve imports.
        if self.auto_update {
            self.analyze_imports(path_idx, ResolutionPhase::Discovery);
        }

        Some(path_idx)
    }

    /// Removes the module with a particular index and all its related characteristics.
    pub fn remove_module(&mut self, path_idx: PathIndex) -> Option<(TypedModule, SymbolTable)> {
        let stale_module = self.module_map.get(path_idx).unwrap();
        let area = SurfaceAreaCalculator::gather_from_module(stale_module, self);
        let module_symbol_idx = stale_module.symbol_idx;
        let module_symbol = self.symbol_library.get(module_symbol_idx)?;
        let module_name = module_symbol.name.clone();
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
        for literal_idx in literals_to_remove {
            self.literals.remove(literal_idx);
        }
        for symbol_idx in area.outer_symbols {
            if let Some(symbol) = self.symbol_library.get_mut(symbol_idx) {
                symbol
                    .references
                    .retain(|reference| reference.module_path != path_idx);
            }
        }
        self.diagnostics
            .retain(|error| error.offending_file != path_idx);
        let parent_directory = get_parent_dir(&stale_module.path_buf)?;
        let dir_map = self.directories.get_mut(parent_directory)?;
        dir_map.remove(&module_name);
        if dir_map.is_empty() {
            self.directories.remove(parent_directory);
        }
        Some((
            self.module_map.remove(path_idx).unwrap(),
            self.symbol_library.remove_module_table(path_idx).unwrap(),
        ))
    }

    /// Changes the content of a single module and updates the entire standpoint accordingly.
    pub fn refresh_module(&mut self, path_idx: PathIndex, text: &str) -> Option<()> {
        let (stale_module, _stale_table) = self.remove_module(path_idx)?;

        let mut update = Module::from_text(&text);
        update.module_path = Some(stale_module.path_buf);

        let new_path_idx = self.add_module(update)?;
        // if it was the Core library that was updated:
        if self
            .corelib_path
            .is_some_and(|core_path| core_path == path_idx)
        {
            self.corelib_path = Some(new_path_idx);
        }
        self.analyze_imports(path_idx, ResolutionPhase::Assessment);
        self.check_module(path_idx);
        // // Refresh related modules.
        // let related_modules = stale_table
        //     .symbols
        //     .into_iter()
        //     .map(|symbol| symbol.references)
        //     .map(|reflists| reflists.into_iter().map(|reflist| reflist.module_path))
        //     .flatten()
        //     .collect::<Vec<_>>();
        // for related_module_idx in related_modules {
        //     if related_module_idx == path_idx {
        //         continue;
        //     }
        //     self.errors.retain(|error| {
        //         !(error.offending_file == related_module_idx
        //             && matches!(
        //                 error.error_type,
        //                 ProgramErrorType::Importing(_) | ProgramErrorType::Typing(_)
        //             ))
        //     });
        //     self.analyze_imports(related_module_idx, ResolutionPhase::Assessment);
        //     self.check_module(related_module_idx);
        // }
        Some(())
    }

    /// Recursively search for and add a module in a directory to the module map.
    pub fn create_module_or_retrieve_nested(
        &mut self,
        dir: &Path,
        module_name: &str,
        phase: ResolutionPhase,
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
                        if phase.is_assessment() {
                            self.add_import_error(error);
                        }
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
                return self.create_module_or_retrieve_nested(&entry_path, module_name, phase);
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
            &mut self.symbol_library,
            &mut self.diagnostics,
            &mut self.literals,
        );
        Some(())
    }

    /// Runs the typechecker on every module in the standpoint.
    pub fn check_all_modules(&mut self) {
        for (_, mut module) in self.module_map.paths_mut() {
            typecheck(
                &mut module,
                &mut self.symbol_library,
                &mut self.diagnostics,
                &mut self.literals,
            );
        }
    }
}
