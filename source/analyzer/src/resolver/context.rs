use super::{symbols::*, ProgramError};
use crate::{Binder, Module, TypedModule};
use std::path::PathBuf;

/// A fully resolved representation of an entire program.
#[derive(Debug)]
pub struct FullProgramContext {
    pub module_paths: Vec<PathBuf>,
    // pub root_folder: Option<PathBuf>,
    // pub directories: HashMap<PathBuf, HashMap<String, usize>>,
    pub symbol_table: SymbolTable,
    pub typed_modules: Vec<TypedModule>,
    pub literals: Vec<Literal>,
    pub errors: Vec<ProgramError>,
    // pub warnings: Vec<Warnings>,
}

impl FullProgramContext {
    /// Builds a program context from the entry module.
    /// It also specifies whether the module imports should be resolved, which adds multiple modules to the context.
    pub fn build_from_module(module: Module, should_resolve_imports: bool) -> Self {
        let mut errors = vec![];
        let mut symbol_table = SymbolTable::new();
        let mut typed_modules = vec![];
        let mut literals = vec![];
        let mut module_paths = vec![];

        let mut binder: Binder = Binder::new(
            module,
            &mut module_paths,
            &mut symbol_table,
            &mut literals,
            &mut errors,
        );
        if let Some(typed_module) = binder.bind() {
            if should_resolve_imports {
                //
            }
            typed_modules.push(typed_module)
        }

        FullProgramContext {
            module_paths,
            symbol_table,
            typed_modules,
            literals,
            errors,
            // root_folder: graph.root_folder,
            // directories: graph.directories,
        }
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
        let context = FullProgramContext::build_from_module(module, false);
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
        let context = FullProgramContext::build_from_module(module, false);
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
        let context = FullProgramContext::build_from_module(module, false);
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
        let context = FullProgramContext::build_from_module(module, false);
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
        let context = FullProgramContext::build_from_module(module, false);
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
        let context = FullProgramContext::build_from_module(module, false);
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
}
