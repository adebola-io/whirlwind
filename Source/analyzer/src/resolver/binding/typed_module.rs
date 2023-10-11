use crate::{PathIndex, SymbolIndex, TypedStmnt};

/// A semantically contextualized module.
#[derive(Debug)]
pub struct TypedModule {
    // pub name: SymbolLocator,
    pub imports: Vec<SymbolIndex>,
    pub line_lengths: Vec<u32>,
    pub path: PathIndex,
    pub statements: Vec<TypedStmnt>,
}
