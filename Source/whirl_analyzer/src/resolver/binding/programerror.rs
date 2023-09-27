use whirl_errors::{ContextError, ImportError, LexError, ParseError, TypeError};

use crate::resolver::symbols::PathIndex;

#[derive(PartialEq, Debug)]
pub struct ProgramError {
    pub offending_file: PathIndex,
    pub error_type: ProgramErrorType,
}

impl ProgramError {
    pub fn contextual(offending_file: PathIndex, error: ContextError) -> Self {
        ProgramError {
            offending_file,
            error_type: ProgramErrorType::Context(error),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ProgramErrorType {
    Lexical(LexError),
    Syntax(ParseError),
    Context(ContextError),
    Importing(ImportError),
    Typing(TypeError),
}
