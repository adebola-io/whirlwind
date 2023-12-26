use crate::PathIndex;
use errors::{ContextError, ImportError, LexError, ParseError, TypeError, Warning};

#[derive(PartialEq, Debug)]
pub struct ProgramDiagnostic {
    pub offending_file: PathIndex,
    pub error_type: DiagnosticType,
}

#[derive(PartialEq, Debug)]
pub enum DiagnosticType {
    Error(Error),
    Warning(Warning),
}

impl ProgramDiagnostic {
    pub fn span(&self) -> ast::Span {
        match &self.error_type {
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
            DiagnosticType::Warning(_) => todo!(),
        }
    }

    pub fn contextual(offending_file: PathIndex, error: ContextError) -> Self {
        ProgramDiagnostic {
            offending_file,
            error_type: DiagnosticType::Error(Error::Context(error)),
        }
    }

    pub fn typing(
        offending_file: PathIndex,
        errortype: errors::TypeErrorType,
        span: ast::Span,
    ) -> ProgramDiagnostic {
        ProgramDiagnostic {
            offending_file,
            error_type: DiagnosticType::Error(Error::Typing(TypeError {
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
