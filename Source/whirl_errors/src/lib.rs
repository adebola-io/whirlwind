mod lex_error;
mod parse_error;
mod type_error;

pub use lex_error::*;
pub use parse_error::*;
pub use type_error::*;
use whirl_ast::Span;

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

pub fn nameless_module() -> TypeError {
    TypeError {
        _type: TypeErrorType::NamelessModule,
        span: Span::default(),
    }
}

pub fn test_in_non_global_scope(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TestInNonGlobalScope,
        span,
    }
}

pub fn invalid_new_expression(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidNewExpression,
        span,
    }
}

pub fn duplicate_constructor(span: whirl_ast::Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DuplicateConstructor,
        span,
    }
}

pub fn unconstructable_model(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnconstructableModel(name),
        span,
    }
}

pub fn mismatched_model_args(
    name: String,
    expected: usize,
    assigned: usize,
    span: whirl_ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedModelArgs {
            name,
            expected,
            assigned,
        },
        span,
    }
}

pub fn uninferrable_parameter(name: String, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UninferrableParameter(name),
        span,
    }
}
