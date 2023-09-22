mod lex_error;
mod parse_error;
mod type_error;

pub use lex_error::*;
pub use parse_error::*;
pub use type_error::*;

#[derive(Debug)]
pub enum ProgramError<'a> {
    ParserError(&'a ParseError),
    LexerError(&'a LexError),
    TypeError(&'a TypeError),
}

pub fn module_declaration_not_global(span: whirl_ast::Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::NonGlobalModuleDeclaration,
        span,
    }
}

pub fn invalid_return(span: whirl_ast::Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::InvalidReturn,
        span,
    }
}

pub fn duplicate_constructor(span: whirl_ast::Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DuplicateConstructor,
        span,
    }
}
