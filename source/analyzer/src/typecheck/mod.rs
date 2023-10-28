mod tests;

use crate::{ProgramError, SymbolTable, TypedModule, TypedStmnt};
use errors::TypeError;

pub struct Typechecker {}

/// Typechecks a module.
pub fn typecheck(
    module: &mut TypedModule,
    symboltable: &mut SymbolTable,
    errors: &mut Vec<ProgramError>,
) {
    for statement in &mut module.statements {
        statements::typecheck_statement(statement, symboltable, errors);
    }
}

mod statements {
    use crate::{SemanticSymbolKind, SymbolIndex, TypedFunctionDeclaration};

    use super::*;

    pub fn typecheck_statement(
        statement: &mut TypedStmnt,
        symboltable: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
    ) -> Option<()> {
        match statement {
            // TypedStmnt::RecordDeclaration => todo!(),
            // TypedStmnt::TestDeclaration(_) => todo!(),
            // TypedStmnt::EnumDeclaration(_) => todo!(),
            // TypedStmnt::UseDeclaration(_) => todo!(),
            // TypedStmnt::VariableDeclaration(_) => todo!(),
            // TypedStmnt::ShorthandVariableDeclaration(_) => todo!(),
            // TypedStmnt::ConstantDeclaration(_) => todo!(),
            // TypedStmnt::TypeDeclaration(_) => todo!(),
            // TypedStmnt::ModelDeclaration(_) => todo!(),
            // TypedStmnt::ModuleDeclaration(_) => todo!(),
            TypedStmnt::FunctionDeclaration(function) => {
                typecheck_function(function, symboltable, errors)
            }
            // TypedStmnt::TraitDeclaration(_) => todo!(),
            // TypedStmnt::ExpressionStatement(_) => todo!(),
            // TypedStmnt::FreeExpression(_) => todo!(),
            // TypedStmnt::ReturnStatement(_) => todo!(),
            // TypedStmnt::BreakStatement(_) => todo!(),
            // TypedStmnt::ForStatement(_) => todo!(),
            // TypedStmnt::WhileStatement(_) => todo!(),
            // TypedStmnt::ContinueStatement(_) => todo!(),
            _ => Some(()),
        }
    }

    fn typecheck_function(
        function: &mut TypedFunctionDeclaration,
        symboltable: &mut SymbolTable,
        errors: &mut Vec<ProgramError>,
    ) -> Option<()> {
        let symbol = symboltable.get_mut(function.name.symbol_idx)?;
        if let SemanticSymbolKind::Function {
            is_public,
            is_async,
            params,
            generic_params,
            return_type,
        } = &mut symbol.kind
        {
            // todo: validate async.
        };
        Some(())
    }
}
