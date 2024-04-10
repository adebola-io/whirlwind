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

pub fn unimplemented_interface(
    offender: String,
    _interface: String,
    base_generic: Option<String>,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnimplementedInterface {
            offender,
            _interface,
            base_generic,
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

pub fn interface_expected(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ExpectedInterface { got: name },
        span,
    }
}

pub fn duplicate_implementation(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::DuplicateImplementationOf { name },
        span,
    }
}

pub fn missing_implementation(interface: &str, method: &str, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MissingImplementation {
            inteface: interface.to_owned(),
            method: method.to_owned(),
        },
        span,
    }
}

pub fn conflicting_implementations(
    previous: (&str, &str),
    duet: (&str, &str),
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::ConflictingImplementation {
            former_interface: previous.0.to_owned(),
            next_interface: duet.1.to_owned(),
            method: previous.1.to_owned(),
        },
        span,
    }
}

pub fn mismatched_generic_params(
    method_name: String,
    expected: usize,
    got: usize,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedGenericParam {
            method_name,
            expected,
            got,
        },
        span,
    }
}

pub fn mismatched_method_access(
    method_name: String,
    got: bool,
    expected: bool,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedMethodAccess {
            method_name,
            got,
            expected,
        },
        span,
    }
}

pub fn mismatched_method_static(
    method_name: String,
    expected: bool,
    got: bool,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedMethodStatic {
            method_name,
            got,
            expected,
        },
        span,
    }
}

pub fn mismatched_method_signature(
    method_name: String,
    left: String,
    right: String,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedMethodSignature {
            method_name,
            left,
            right,
        },
        span,
    }
}

pub fn failing_clause(base: String, method: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::FailedClause { base, method },
        span,
    }
}

pub fn mismatched_constraint(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedConstraint,
        span,
    }
}

pub fn missing_constraint(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MissingConstraint,
        span,
    }
}

pub fn unexpected_constraint(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnexpectedConstraint,
        span,
    }
}

pub fn self_reference(valuename: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::SelfReference { valuename },
        span,
    }
}

pub fn import_in_non_global_scope(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::ExternImportInNonGlobalScope,
        span,
    }
}

pub fn duplicate_import_name(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::DuplicateImportName { name },
        span,
    }
}

pub fn generic_function_import(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::GenericFunctionImport,
        span,
    }
}

pub fn unlabelled_parameter(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnlabelledParameter { name },
        span,
    }
}
