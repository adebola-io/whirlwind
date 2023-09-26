use crate::ModuleGraph;

use super::binding::StatementBinding;
use std::path::PathBuf;
use whirl_ast::{Literal, SemanticSymbolDeclaration, SemanticSymbolReference, SymbolTable};
use whirl_errors::ContextError;

/// A representation of the program.
pub struct FullProgramContext {
    pub module_paths: Vec<PathBuf>,
    pub symbol_table: SymbolTable,
    pub statement_bindings: Vec<StatementBinding>,
    pub literals: Literal,
    pub errors: Vec<ContextError>,
}

impl FullProgramContext {
    pub fn new(graph: ModuleGraph) -> Self {
        todo!()
    }
    /// Returns the first declaration of a symbol.
    pub fn get_declaration_of(&self, index: usize) -> Option<SemanticSymbolDeclaration> {
        let symbol = self.symbol_table.get_symbol(index)?;
        let first_reference = symbol.references.first()?;
        Some(SemanticSymbolDeclaration {
            module_path: self.module_paths.get(first_reference.module_path.0)?,
            span: &symbol.declaration_range,
        })
    }
    /// Find all references to a symbol using its index.
    pub fn find_all_references(
        &self,
        index: usize,
    ) -> Option<impl Iterator<Item = SemanticSymbolReference>> {
        Some(
            self.symbol_table
                .get_symbol(index)?
                .references
                .iter()
                .map(|list| {
                    list.starts
                        .iter()
                        .map(|start_position| SemanticSymbolReference {
                            module_path: &self.module_paths[list.module_path.0],
                            start_position: *start_position,
                        })
                })
                .flatten(),
        )
    }
}
