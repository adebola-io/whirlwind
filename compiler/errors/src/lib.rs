mod bytecode_error;
mod context_error;
mod import_error;
mod lex_error;
mod parse_error;
mod runtime_error;
mod stringify;
mod type_error;
mod warning;

pub use bytecode_error::*;
pub use context_error::*;
pub use import_error::*;
pub use lex_error::*;
pub use parse_error::*;
pub use runtime_error::*;
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

pub fn method_in_constructor(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MethodInConstructor,
        span,
    }
}

pub fn not_orderable(operator: ast::BinOperator, name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NotOrderable { name, operator },
        span,
    }
}

pub fn not_sequenced(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NotSequenced { name },
        span,
    }
}

pub fn unimplemented_interface(offender: String, _interface: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnimplementedInterface {
            offender,
            _interface,
        },
        span,
    }
}

pub fn numeric_exclusive_operation(typ: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NumericExclusiveOperation { typ },
        span,
    }
}
