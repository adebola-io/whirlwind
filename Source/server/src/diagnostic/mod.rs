use ast::Span;
use errors::{LexErrorPos, ModuleError};
use printer::{
    stringify_import_error, stringify_lex_error, stringify_parse_error, stringify_type_error,
};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Convert a program error to a diagnostic.
pub fn to_diagnostic(error: &ModuleError) -> Diagnostic {
    let (message, range) = match error {
        ModuleError::LexerError(lex_error) => (
            stringify_lex_error(&lex_error.error_type),
            to_range(match lex_error.position {
                LexErrorPos::Point(pos) => Span::at(pos),
                LexErrorPos::Span(span) => span,
            }),
        ),
        ModuleError::ParserError(parse_error) => (
            stringify_parse_error(&parse_error._type),
            to_range(parse_error.span),
        ),
        ModuleError::TypeError(type_error) => (
            stringify_type_error(&type_error._type),
            to_range(type_error.span),
        ),
        ModuleError::ImportError(resolve_error) => (
            stringify_import_error(&resolve_error._type),
            to_range(resolve_error.span.unwrap_or_default()),
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
