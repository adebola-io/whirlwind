use ast::UseTarget;

use crate::{PathIndex, SymbolIndex, TypedStmnt};
use std::path::PathBuf;

/// A semantically contextualized module.
#[derive(Debug)]
pub struct TypedModule {
    pub line_lengths: Vec<u32>,
    pub path_buf: PathBuf,
    pub path_idx: PathIndex,
    pub symbol_idx: SymbolIndex,
    pub statements: Vec<TypedStmnt>,
    // todo: ugh.
    pub imports: Vec<(UseTarget, Vec<SymbolIndex>)>,
}
