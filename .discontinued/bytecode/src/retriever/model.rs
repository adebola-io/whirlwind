use analyzer::{Standpoint, SymbolIndex};

/// Returns a type declaration that maps to a type symbol.
pub struct ModelRetriever<'a> {
    standpoint: &'a Standpoint,
    symbolindex: SymbolIndex,
}
