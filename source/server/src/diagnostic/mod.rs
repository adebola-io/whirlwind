use analyzer::{ProgramError, ProgramErrorType};
use ast::Span;
use errors::LexErrorPos;
use printer::{
    stringify_context_error, stringify_import_error, stringify_lex_error, stringify_parse_error,
    stringify_type_error,
};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Convert a program error to a diagnostic.
pub fn to_diagnostic(error: &ProgramError) -> Diagnostic {
    let (message, range) = match &error.error_type {
        ProgramErrorType::Lexical(lex_error) => (
            stringify_lex_error(&lex_error.error_type),
            to_range(match lex_error.position {
                LexErrorPos::Point(pos) => Span::at(pos),
                LexErrorPos::Span(span) => span,
            }),
        ),
        ProgramErrorType::Syntax(parse_error) => (
            stringify_parse_error(&parse_error._type),
            to_range(parse_error.span),
        ),
        ProgramErrorType::Typing(type_error) => (
            stringify_type_error(&type_error._type),
            to_range(type_error.span),
        ),
        ProgramErrorType::Importing(resolve_error) => (
            stringify_import_error(&resolve_error._type),
            to_range(resolve_error.span.unwrap_or_default()),
        ),
        ProgramErrorType::Context(context_error) => (
            stringify_context_error(&context_error._type),
            to_range(context_error.span),
        ),
    };
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some(format!("whirlwind")),
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a span to a range.
pub fn to_range(span: Span) -> Range {
    Range {
        start: Position {
            line: span.start[0] - 1,
            character: span.start[1] - 1,
        },
        end: Position {
            line: span.end[0] - 1,
            character: span.end[1] - 1,
        },
    }
}
