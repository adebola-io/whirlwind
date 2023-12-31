use crate::PathIndex;
use errors::{ContextError, ImportError, LexError, ParseError, TypeError, Warning};

#[derive(PartialEq, Debug)]
pub struct ProgramDiagnostic {
    pub offending_file: PathIndex,
    pub _type: DiagnosticType,
}

#[derive(PartialEq, Debug)]
pub enum DiagnosticType {
    Error(Error),
    Warning(Warning),
}

impl ProgramDiagnostic {
    pub fn span(&self) -> ast::Span {
        match &self._type {
            DiagnosticType::Error(error) => match error {
                Error::Lexical(le) => match le.position {
                    errors::LexErrorPos::Point(point) => ast::Span::at(point),
                    errors::LexErrorPos::Span(span) => span,
                },
                Error::Syntax(syntax) => syntax.span,
                Error::Context(ctx) => ctx.span,
                Error::Importing(import) => import.span.unwrap_or_default(),
                Error::Typing(typing) => typing.span,
            },
            DiagnosticType::Warning(warning) => warning.span,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self._type, DiagnosticType::Error(_))
    }

    pub fn is_warning(&self) -> bool {
        matches!(self._type, DiagnosticType::Warning(_))
    }

    pub fn contextual(offending_file: PathIndex, error: ContextError) -> Self {
        ProgramDiagnostic {
            offending_file,
            _type: DiagnosticType::Error(Error::Context(error)),
        }
    }

    pub fn typing(
        offending_file: PathIndex,
        errortype: errors::TypeErrorType,
        span: ast::Span,
    ) -> ProgramDiagnostic {
        ProgramDiagnostic {
            offending_file,
            _type: DiagnosticType::Error(Error::Typing(TypeError {
                _type: errortype,
                span,
            })),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Error {
    Lexical(LexError),
    Syntax(ParseError),
    Context(ContextError),
    Importing(ImportError),
    Typing(TypeError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::Context(context_err) => context_err._type.to_string(),
                Error::Importing(import_err) => import_err._type.to_string(),
                Error::Lexical(lex_err) => lex_err.error_type.to_string(),
                Error::Syntax(syntax) => syntax._type.to_string(),
                Error::Typing(type_error) => type_error._type.to_string(),
            }
        )
    }
}

impl std::fmt::Display for ProgramDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self._type {
                DiagnosticType::Error(error) => error.to_string(),
                DiagnosticType::Warning(warning) => warning.warning_type.to_string(),
            }
        )
    }
}
