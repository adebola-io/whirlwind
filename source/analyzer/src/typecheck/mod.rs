use crate::{SymbolTable, TypedModule, TypedStmnt};
use errors::TypeError;

pub struct Typechecker {}

/// Typechecks a module.
pub fn typecheck(module: &mut TypedModule, symboltable: &mut SymbolTable) -> Vec<TypeError> {
    let mut errors = vec![];
    for statement in &mut module.statements {
        errors.append(&mut statements::typecheck_statement(statement, symboltable))
    }
    errors
}

mod statements {
    use super::*;

    pub fn typecheck_statement(
        _statement: &mut TypedStmnt,
        _symboltable: &mut SymbolTable,
    ) -> Vec<TypeError> {
        vec![]
    }
}
