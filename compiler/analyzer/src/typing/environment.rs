use std::collections::HashMap;

use crate::{EvaluatedType, SymbolIndex};

/// An environment is a scoped area in which a type clause is assumed to be
/// satisfied. It is more or less a shadow of the symbol library.
pub struct TypeEnvironment {
    generics: HashMap<SymbolIndex, Vec<Assumption>>,
}

/// A temporary benefit of doubt for a type constraint.
pub enum Assumption {
    /// X is Y.
    Equality(EvaluatedType),
    /// A implements B.
    Implementation(EvaluatedType),
}

impl TypeEnvironment {
    /// Returns an implementation
    pub fn get_implementation_of(target_interface: SymbolIndex) -> Option<EvaluatedType> {
        todo!()
    }
}
