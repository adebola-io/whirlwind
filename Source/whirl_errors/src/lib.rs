mod lex_error;
mod parse_error;
mod proj_error;
mod type_error;

pub use lex_error::*;
pub use parse_error::*;
pub use proj_error::*;
pub use type_error::*;

#[derive(Debug)]
pub enum ProgramError {
    ParserError(ParseError),
    LexerError(LexError),
    TypeError(TypeError),
    ProjectError(ProjectError),
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
        span,
    }
}

pub fn global_control(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::GlobalControl,
        span,
    }
}

pub fn nameless_module() -> ProgramError {
    ProgramError::ProjectError(ProjectError {
        _type: ProjectErrorType::NamelessModule,
        span: None,
    })
}

pub fn test_in_non_global_scope(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TestInNonGlobalScope,
        span,
    }
}
