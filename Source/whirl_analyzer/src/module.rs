use whirl_ast::{ScopeManager, Statement};
use whirl_errors::ProgramError;

use crate::type_check_text;

/// A completely parsed program.
#[derive(Debug)]
pub struct Module {
    pub program_errors: Vec<ProgramError>,
    pub scope_manager: ScopeManager,
    pub statements: Vec<Statement>,
}

impl From<&str> for Module {
    fn from(value: &str) -> Self {
        let mut checker = type_check_text(value);

        let mut program_errors = vec![];
        loop {
            match checker.next() {
                Some(statement_errors) => {
                    for error in statement_errors {
                        program_errors.push(ProgramError::TypeError(error))
                    }
                }
                None => break,
            }
        }
        for syntax_error in std::mem::take(&mut checker.syntax_errors) {
            program_errors.push(ProgramError::ParserError(syntax_error))
        }

        for lex_error in std::mem::take(&mut checker.lexical_errors) {
            program_errors.push(ProgramError::LexerError(lex_error))
        }

        Module {
            scope_manager: std::mem::take(checker.scope_manager()),
            program_errors,
            statements: std::mem::take(&mut checker.statements),
        }
    }
}
