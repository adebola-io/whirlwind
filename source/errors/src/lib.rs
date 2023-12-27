mod bytecode_error;
mod context_error;
mod import_error;
mod lex_error;
mod parse_error;
mod stringify;
mod type_error;
mod warning;

pub use bytecode_error::*;
pub use context_error::*;
pub use import_error::*;
pub use lex_error::*;
pub use parse_error::*;
pub use stringify::*;
pub use type_error::*;
pub use warning::*;

/// An error within a module.
#[derive(Debug)]
pub enum ModuleError<'a> {
    ParserError(&'a ParseError),
    LexerError(&'a LexError),
    TypeError(&'a TypeError),
    ImportError(&'a ImportError),
}

pub fn unused_import_symbol(name: String, span: ast::Span) -> Warning {
    Warning {
        span,
        warning_type: WarningType::UnusedImportSymbol(name),
    }
}

pub fn unused_model_symbol(name: String, span: ast::Span) -> Warning {
    Warning {
        span,
        warning_type: WarningType::UnusedModelSymbol(name),
    }
}
