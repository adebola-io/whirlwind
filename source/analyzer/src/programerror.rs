use crate::PathIndex;
use errors::{ContextError, ImportError, LexError, ParseError, TypeError};

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

    pub fn typing(
        offending_file: PathIndex,
        errortype: errors::TypeErrorType,
        span: ast::Span,
    ) -> ProgramError {
        ProgramError {
            offending_file,
            error_type: ProgramErrorType::Typing(TypeError {
                _type: errortype,
                span,
            }),
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
