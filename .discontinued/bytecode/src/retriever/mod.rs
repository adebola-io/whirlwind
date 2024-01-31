mod function;
mod type;

use analyzer::{Standpoint, SymbolIndex};
pub use function::FunctionRetriever;

// /// Returns an interface declaration that maps to an interface symbol.
// pub struct InterfaceRetriever<'a> {
//     standpoint: &'a Standpoint,
//     symbolindex: SymbolIndex,
// }

// /// Returns a method declaration that maps to a method symbol.
// pub struct MethodRetriever<'a> {
//     standpoint: &'a Standpoint,
//     symbolindex: SymbolIndex,
// }
