use super::{symbols::*, ProgramError};
use crate::{Binder, ModuleGraph, TypedModule};
use std::{collections::HashMap, path::PathBuf};

/// A fully resolved representation of an entire program.
#[derive(Debug)]
pub struct FullProgramContext {
    pub module_paths: Vec<PathBuf>,
    pub root_folder: Option<PathBuf>,
    pub directories: HashMap<PathBuf, HashMap<String, usize>>,
    pub symbol_table: SymbolTable,
    pub typed_modules: Vec<TypedModule>,
    pub literals: Vec<Literal>,
    pub errors: Vec<ProgramError>,
    // pub warnings: Vec<Warnings>,
}

#[derive(Debug)]
pub struct ModuleGraphSkeleton {
    pub graph: ModuleGraph,
}

impl FullProgramContext {
    /// Build a program context from a module graph.
    pub fn build_from_graph(mut graph: ModuleGraph) -> Self {
        let mut errors = vec![];
        let mut symbol_table = SymbolTable::new();
        let mut typed_modules = vec![];
        let mut literals = vec![];
        let mut module_paths = vec![];

        let modules = std::mem::take(&mut graph.modules);

        for module in modules {
            let mut binder: Binder = Binder::new(
                module,
                &graph,
                &mut module_paths,
                &mut symbol_table,
                &mut literals,
                &mut errors,
            );
            if let Some(typed_module) = binder.bind() {
                typed_modules.push(typed_module)
            }
        }

        FullProgramContext {
            module_paths,
            symbol_table,
            typed_modules,
            literals,
            errors,
            root_folder: graph.root_folder,
            directories: graph.directories,
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
    // todo: use virtual fs.
    use crate::{resolve_modules, FullProgramContext, PathIndex, SymbolIndex};

    #[test]
    fn test_shorthand_variable_context() {
        let graph = resolve_modules("../../Tests/binding/variables.wrl");
        let context = FullProgramContext::build_from_graph(graph);
        assert_eq!(
            context.symbol_table.get(SymbolIndex(0)).unwrap().name,
            "name"
        );
        assert_eq!(
            context.symbol_table.get(SymbolIndex(1)).unwrap().name,
            "CONSTANT_VALUE"
        );
        assert_eq!(
            context.symbol_table.get(SymbolIndex(2)).unwrap().name,
            "Number"
        );
    }

    #[test]
    fn test_call_expression_bind() {
        let graph = resolve_modules("../../Tests/binding/this_and_call.wrl");
        let _context = FullProgramContext::build_from_graph(graph);
        // println!(
        //     "{:#?}",
        //     context
        //         .symbol_table
        //         .in_module(PathIndex(0))
        //         .map(|symbol| &symbol.name)
        //         .collect::<Vec<_>>()
        // );
    }

    #[test]
    fn test_model_bind() {
        let graph = resolve_modules("../../Tests/binding/types.wrl");
        let context = FullProgramContext::build_from_graph(graph);
        println!(
            "{:#?}",
            context
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.symbol_kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            context
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn test_this_type() {
        let graph = resolve_modules("../../Tests/binding/this_type.wrl");
        let context = FullProgramContext::build_from_graph(graph);
        println!(
            "{:#?}",
            context
                .symbol_table
                .in_module(PathIndex(0))
                .map(|symbol| (&symbol.name, &symbol.symbol_kind, &symbol.references))
                .collect::<Vec<_>>()
        );
        println!(
            "ERRORS: \n\n\n{:#?}",
            context
                .errors
                .iter()
                .filter(|error| error.offending_file == PathIndex(0))
                .collect::<Vec<_>>()
        )
    }
}
