mod lex_error;
mod parse_error;
mod resolve_error;
mod type_error;

use std::path::PathBuf;

pub use lex_error::*;
pub use parse_error::*;
pub use resolve_error::*;
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

pub fn empty_path_list(span: whirl_ast::Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::EmptyPathList,
        span,
    }
}

pub fn unknown_file_type(path_buf: PathBuf) -> ResolveError {
    ResolveError {
        _type: ResolveErrorType::UnknownFileType { path_buf },
        span: None,
    }
}

pub fn error_reading_entry_file(error: std::io::Error) -> ResolveError {
    ResolveError {
        _type: ResolveErrorType::ErrorReadingEntry(error),
        span: None,
    }
}

pub fn cannot_resolve_from_global_file() -> ResolveError {
    ResolveError {
        _type: ResolveErrorType::ResolvingFromGlobalFile,
        span: None,
    }
}

pub fn io_error(error: std::io::Error) -> ResolveError {
    ResolveError {
        _type: ResolveErrorType::VagueAccessError(error),
        span: None,
    }
}
