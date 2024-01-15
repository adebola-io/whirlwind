#![allow(unused)]
use std::collections::HashMap;

use crate::{EvaluatedType, SymbolIndex};

/// An environment is a scoped area in which a type clause is assumed to be
/// satisfied. It is more or less a shadow of the symbol library.
pub struct TypeEnvironment {
    assumptions: Vec<Assumption>,
}

/// A temporary benefit of doubt for a type constraint.
pub struct Assumption {
    /// The (most definitely) generic type that is being supposed against.
    base: SymbolIndex,
    /// Possible variants of the type.
    /// When the logical operation `T is U or T is V` is assumed,
    /// it produces the opaque typeform U | V for T.
    type_forms: Vec<EvaluatedType>,
    implementations: Vec<EvaluatedType>,
}

impl TypeEnvironment {
    /// Creates a new type environment.
    pub fn new() -> Self {
        TypeEnvironment {
            assumptions: vec![],
        }
    }
    /// Returns an implementation of an interface on a type,
    /// if the environment has a provision for it.
    pub fn get_implementation_of(
        &self,
        target_interface: SymbolIndex,
        typ: &EvaluatedType,
    ) -> Option<EvaluatedType> {
        todo!()
    }
    /// Returns the possible type forms of a type in the current environment,
    pub fn get_possible_type_forms(
        &self,
        base: SymbolIndex,
    ) -> Option<impl Iterator<Item = EvaluatedType>> {
        Some(vec![].into_iter())
    }
    /// Returns the methods of a
    pub fn get_methods_of(&self) -> Option<EvaluatedType> {
        todo!()
    }
}
