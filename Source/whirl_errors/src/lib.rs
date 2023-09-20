mod lex_error;
mod parse_error;
mod type_error;

pub use lex_error::*;
pub use parse_error::*;
pub use type_error::*;

#[derive(Debug)]
pub enum ProgramError {
    ParserError(ParseError),
    LexerError(LexError),
    TypeError(TypeError),
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

pub fn unknown_variable_in_scope(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnknownVariableInScope { name },
        spans: vec![span],
    }
}

pub fn global_control(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::GlobalControl,
        spans: vec![span],
    }
}
