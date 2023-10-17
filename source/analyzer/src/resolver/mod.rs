use std::{collections::HashMap, path::PathBuf};

use ast::UseTarget;

use crate::{PathIndex, SymbolIndex};

/// The import resolver is responsible for linking declarations and symbols across files and directories.
pub struct ImportResolver {
    /// The module requesting all the imports.
    root: PathIndex,
    /// The routes to follow and the import indexes to solve for.
    targets: Vec<(UseTarget, Vec<SymbolIndex>)>,
}

impl ImportResolver {
    pub fn resolve(
        root: PathIndex,
        targets: Vec<(UseTarget, Vec<SymbolIndex>)>,
        directories: &mut HashMap<PathBuf, HashMap<String, PathIndex>>,
    ) {
        let resolver = ImportResolver { root, targets };
    }
}
