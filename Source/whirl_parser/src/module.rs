use crate::{errors, parse_text};
use whirl_ast::{ScopeManager, Span, Statement};
use whirl_lexer::Lexer;

use errors::ProgramError;
/// A completely parsed program.
#[derive(Debug)]
pub struct Module {
    pub errors: Vec<ProgramError>,
    pub scope_manager: ScopeManager,
    pub statements: Vec<Statement>,
}

impl From<&str> for Module {
    fn from(value: &str) -> Self {
        let mut parser = parse_text(value);

        let mut statements: Vec<Statement> = vec![];
        let mut errors: Vec<ProgramError> = std::mem::take(parser.lexer.borrow_mut().errors())
            .into_iter()
            .map(|error| ProgramError::LexerError(error))
            .collect();

        loop {
            match parser.next() {
                Some(result) => match result {
                    Ok(statement) => statements.push(statement),
                    Err(e) => errors.push(ProgramError::ParserError(e)),
                },
                None => break,
            }
        }

        Module {
            scope_manager: std::mem::take(parser.scope_manager()),
            errors,
            statements,
        }
    }
}
