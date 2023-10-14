use crate::{PathIndex, TypedStmnt};

/// A semantically contextualized module.
#[derive(Debug)]
pub struct TypedModule {
    // pub name: SymbolLocator,
    pub line_lengths: Vec<u32>,
    pub path: PathIndex,
    pub statements: Vec<TypedStmnt>,
}
