use analyzer::{DiagnosticType, Error, ProgramDiagnostic};
use ast::Span;
use errors::LexErrorPos;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Convert a program diagnostic to an LSP diagnostic.
pub fn progdiagnostic_to_diagnostic(diagnostic: &ProgramDiagnostic) -> Diagnostic {
    let (message, range, severity) = match &diagnostic._type {
        DiagnosticType::Error(error) => {
            let (message, range) = match error {
                Error::Lexical(lex_error) => (
                    lex_error.error_type.to_string(),
                    to_range(match lex_error.position {
                        LexErrorPos::Point(pos) => Span::at(pos),
                        LexErrorPos::Span(span) => span,
                    }),
                ),
                Error::Syntax(parse_error) => {
                    (parse_error._type.to_string(), to_range(parse_error.span))
                }
                Error::Typing(type_error) => {
                    (type_error._type.to_string(), to_range(type_error.span))
                }
                Error::Importing(resolve_error) => (
                    resolve_error._type.to_string(),
                    to_range(resolve_error.span.unwrap_or_default()),
                ),
                Error::Context(context_error) => (
                    context_error._type.to_string(),
                    to_range(context_error.span),
                ),
            };
            (message, range, Some(DiagnosticSeverity::ERROR))
        }
        DiagnosticType::Warning(warning) => {
            let range = to_range(warning.span);
            let message = warning.warning_type.to_string();
            (message, range, Some(DiagnosticSeverity::WARNING))
        }
    };
    Diagnostic {
        range,
        severity,
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
