use analyzer::{Standpoint, SymbolIndex};

/// Returns a model declaration that maps to a model symbol.
pub struct ModelRetriever<'a> {
    standpoint: &'a Standpoint,
    symbolindex: SymbolIndex,
}
