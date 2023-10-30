#![allow(unused)]

use crate::{EvaluatedType, IntermediateType, SymbolIndex, SymbolTable};
use errors::TypeErrorType;

pub fn unify(
    left: &EvaluatedType,
    right: &EvaluatedType,
    symboltable: &SymbolTable,
) -> Result<EvaluatedType, TypeErrorType> {
    /// Types are directly equal.
    if left == right {
        return Ok(left.clone());
    }
    Err(TypeErrorType::MismatchedAssignment {
        left: symboltable.format_evaluated_type(left),
        right: symboltable.format_evaluated_type(right),
    })
}

pub fn try_unify(
    left: &EvaluatedType,
    right: &EvaluatedType,
    symboltable: &SymbolTable,
) -> Option<EvaluatedType> {
    if left == right {
        return Some(left.clone());
    }
    None
}

/// Converts an intermediate type into an evaluation.
pub fn evaluate(
    typ: &IntermediateType,
    symboltable: &SymbolTable,
    // A map of the solutions for any encountered generic types.
    solved_generics: Option<&Vec<(&SymbolIndex, &EvaluatedType)>>,
) -> EvaluatedType {
    EvaluatedType::Unknown
}
