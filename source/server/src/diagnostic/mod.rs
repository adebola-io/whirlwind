use analyzer::{ProgramError, ProgramErrorType};
use ast::Span;
use errors::LexErrorPos;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Convert a program error to a diagnostic.
pub fn error_to_diagnostic(error: &ProgramError) -> Diagnostic {
    let (message, range) = match &error.error_type {
        ProgramErrorType::Lexical(lex_error) => (
            lex_error.error_type.to_string(),
            to_range(match lex_error.position {
                LexErrorPos::Point(pos) => Span::at(pos),
                LexErrorPos::Span(span) => span,
            }),
        ),
        ProgramErrorType::Syntax(parse_error) => {
            (parse_error._type.to_string(), to_range(parse_error.span))
        }
        ProgramErrorType::Typing(type_error) => {
            (type_error._type.to_string(), to_range(type_error.span))
        }
        ProgramErrorType::Importing(resolve_error) => (
            resolve_error._type.to_string(),
            to_range(resolve_error.span.unwrap_or_default()),
        ),
        ProgramErrorType::Context(context_error) => (
            context_error._type.to_string(),
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
pub fn to_range(mut span: Span) -> Range {
    if span.start[0] > 0 {
        span.start[0] -= 1
    }
    if span.start[1] > 0 {
        span.start[1] -= 1
    }
    if span.end[0] > 0 {
        span.end[0] -= 1
    }
    if span.end[1] > 0 {
        span.end[1] -= 1
    }
    Range {
        start: Position {
            line: span.start[0],
            character: span.start[1],
        },
        end: Position {
            line: span.end[0],
            character: span.end[1],
        },
    }
}
