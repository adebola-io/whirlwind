use utils::get_parent_dir;

use super::{symbols::*, ProgramError};
use crate::{Binder, Module, ProgramErrorType::Importing, TypedModule};
use std::{collections::HashMap, path::PathBuf};

/// A fully resolved representation of an entire program.
#[derive(Debug)]
pub struct FullProgramContext {
    pub module_paths: Vec<PathBuf>,
    pub root_folder: Option<PathBuf>,
    pub entry_module: PathIndex,
    pub directories: HashMap<PathBuf, HashMap<String, usize>>,
    pub symbol_table: SymbolTable,
    pub typed_modules: Vec<TypedModule>,
    pub literals: Vec<Literal>,
    pub errors: Vec<ProgramError>,
    pub should_resolve_imports: bool,
    // pub warnings: Vec<Warnings>,
}

impl FullProgramContext {
    pub fn new(should_resolve_imports: bool) -> Self {
        let context = FullProgramContext {
            module_paths: vec![],
            entry_module: PathIndex(0),
            directories: HashMap::new(),
            symbol_table: SymbolTable::new(),
            typed_modules: vec![],
            literals: vec![],
            errors: vec![],
            root_folder: None,
            should_resolve_imports,
        };
        context
    }
    /// Builds a program context from the entry module.
    /// It also specifies whether the module imports should be resolved, which adds multiple modules to the context.
    pub fn build_from_module(module: Module, should_resolve_imports: bool) -> Option<Self> {
        let mut context = FullProgramContext::new(should_resolve_imports);
        context.add_module(module);
        // for import in import_lists {
        //
        // }
        Some(context)
    }
    /// Returns the first declaration of a symbol.
    pub fn get_declaration_of(&self, index: SymbolIndex) -> Option<SemanticSymbolDeclaration> {
        let symbol = self.symbol_table.get(index)?;
        let first_reference = symbol.references.first()?;
        Some(SemanticSymbolDeclaration {
            module_path: self
                .module_paths
                .get(first_reference.module_path.0 as usize)?,
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
                            module_path: &self.module_paths[list.module_path.0 as usize],
                            start_position: *start_position,
                        })
                })
                .flatten(),
        )
    }

    pub fn add_module(&mut self, module: Module) -> Option<()> {
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

        let mut import_lists = vec![];

        let mut binder: Binder = Binder::new(
            module,
            &mut self.module_paths,
            &mut self.symbol_table,
            &mut self.literals,
            &mut self.errors,
        );
        if self.should_resolve_imports {
            if let Some((typed_module, imports)) = binder.bind_and_show_imports() {
                import_lists.push(imports);
                self.typed_modules.push(typed_module);
            };
        } else {
            if let Some(typed_module) = binder.bind() {
                self.typed_modules.push(typed_module);
            }
        };

        Some(())
    }
    /// Check if a folder is part of the context.
    pub fn contains_folder(&self, dir: &std::path::Path) -> bool {
        self.directories.get(dir).is_some()
    }
    /// Add an import error to the list of errors.
    pub fn add_import_error(&mut self, error: errors::ImportError) {
        self.errors.push(ProgramError {
            offending_file: self.entry_module,
            error_type: Importing(error),
        })
    }
    /// Check if the context is empty.
    pub fn is_empty(&self) -> bool {
        self.typed_modules.is_empty()
    }
    /// Check if a file is part of the context.
    pub fn contains_file(&self, path_buf: &PathBuf) -> bool {
        self.module_paths.contains(&path_buf)
    }
    /// Returns reference to a (typed) module at a specific path, if it exists.
    pub fn get_module_at_path(&self, path_buf: &PathBuf) -> Option<&TypedModule> {
        let index = self
            .module_paths
            .iter()
            .enumerate()
            .find(|tuple| tuple.1 == path_buf)?
            .0;
        self.typed_modules.get(index)
    }
    /// Return mutable reference to a (typed) module at a specific path, if it exists.
    pub fn get_mut_module_at_path(&mut self, path_buf: &PathBuf) -> Option<&mut TypedModule> {
        let index = self
            .module_paths
            .iter()
            .enumerate()
            .find(|tuple| tuple.1 == path_buf)?
            .0;
        self.typed_modules.get_mut(index)
    }
    /// Changes the content of a single module and updates the entire context accordingly.
    pub fn refresh_module_with_text(&mut self, path: &PathBuf, text: String) -> Option<()> {
        let index = self
            .module_paths
            .iter()
            .enumerate()
            .find(|tuple| tuple.1 == path)?
            .0;
        let path_idx = PathIndex(index as u32);
        let mut affected_symbols = vec![];
        let mut affected_modules = vec![];
        // todo: find more performant ways to implement this.
        // Get all symbols that were declared in the module to refresh.
        for (index, symbol) in self.symbol_table.symbols().enumerate() {
            if symbol.references[0].module_path == path_idx {
                let symbol_idx = SymbolIndex(index);
                affected_symbols.push(symbol_idx);
                // Get all the connected modules, so they can also be updated in real time.
                // It should skip over this module itself.
                for reference in self.symbol_table.get(symbol_idx)?.references.iter() {
                    if reference.module_path == path_idx {
                        continue;
                    }
                    affected_modules.push(reference.module_path);
                }
            }
        }

        // Delete the affected symbols and refresh.
        for symbol_idx in affected_symbols {
            self.symbol_table.remove(symbol_idx);
        }
        // Delete stale errors.
        self.errors.retain(|error| error.offending_file != path_idx);

        // How to replace the stale module?
        // First add the updated module normally at the end of the list,
        // then swap remove it to its actual position.
        // todo: what if the module name changes?
        let mut update = Module::from_text(text, index);
        update.module_path = Some(path.clone());
        self.should_resolve_imports = false;
        self.add_module(update);
        self.should_resolve_imports = true;
        self.typed_modules.swap_remove(index);

        // Update related modules.
        affected_modules
            .iter()
            .filter_map(|idx| self.module_paths.get(idx.0 as usize))
            .for_each(|_| todo!());

        Some(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    // todo: use virtual fs.
    use crate::{FullProgramContext, Module, PathIndex};

    #[test]
    fn bind_variables_and_constants() {
        let text = "
            module Test; 

            public function Main() {
                greeting := \"Say Hello\";
                const CONSTANT: Number = 9090;
            }
        ";
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, false).unwrap();
        assert!(context.errors.len() == 1);
        assert!(context
            .symbol_table
            .find(|symbol| symbol.name == "greeting")
            .is_some());
        assert!(context
            .symbol_table
            .find(|symbol| symbol.name == "CONSTANT")
            .is_some());
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
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, false).unwrap();
        assert!(context.errors.len() == 1);
        assert!(context
            .symbol_table
            .find(|symbol| symbol.name == "Println")
            .is_some());
        assert!(context
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
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, false).unwrap();
        assert!(context
            .symbol_table
            .find(|symbol| symbol.name == "Car")
            .is_some());
        assert!(context
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
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, false).unwrap();
        assert!(context.errors.len() == 0);
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
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, false).unwrap();
        println!(
            "{:#?}",
            context
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            context
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        );
        assert!(context.errors.len() == 0);
    }

    #[test]
    fn test_fn_expr() {
        let text = "
            module Test;

            function Main() {
                square := fn(a) a * 2;
            }
        ";
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, false).unwrap();
        println!(
            "{:#?}",
            context
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            context
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        );
        assert!(context.errors.len() == 0);
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
        let mut module = Module::from_text(format!("{text}"), 0);
        module.module_path = Some(PathBuf::from("testing://"));
        let context = FullProgramContext::build_from_module(module, true).unwrap();
        assert!(context
            .symbol_table
            .find(|symbol| symbol.name == "Println")
            .is_some());
        assert!(context
            .symbol_table
            .find(|symbol| symbol.name == "Main")
            .is_some());
        assert!(context.errors.len() == 0);
    }
}
