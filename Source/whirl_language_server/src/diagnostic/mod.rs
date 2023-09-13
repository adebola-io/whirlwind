use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use whirl_ast::{ScopeManager, Span};
use whirl_errors::{LexErrorPos, ProgramError};
use whirl_printer::{stringify_lex_error, stringify_parse_error, stringify_type_error};

/// Convert a program error to a diagnostic.
pub fn to_diagnostic(scope_manager: &ScopeManager, error: &ProgramError) -> Diagnostic {
    let (message, range) = match error {
        ProgramError::LexerError(lex_error) => (
            stringify_lex_error(&lex_error.error_type),
            to_range(match lex_error.position {
                LexErrorPos::Point(pos) => Span::at(pos),
                LexErrorPos::Span(span) => span,
            }),
        ),
        ProgramError::ParserError(parse_error) => (
            stringify_parse_error(&parse_error.error_type),
            to_range(parse_error.span),
        ),
        ProgramError::TypeError(type_error) => (
            stringify_type_error(scope_manager, &type_error._type),
            to_range(type_error.spans[0]),
        ),
    };
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some(format!("whirl")),
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
            line: span.start[0],
            character: span.start[1],
        },
        end: Position {
            line: span.end[0],
            character: span.end[1],
        },
    }
}
