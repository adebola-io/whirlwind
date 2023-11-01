mod context_error;
mod import_error;
mod lex_error;
mod parse_error;
mod type_error;

use std::path::PathBuf;

use ast::UseTarget;
pub use context_error::*;
pub use import_error::*;
pub use lex_error::*;
pub use parse_error::*;
pub use type_error::*;

/// An error within a module.
#[derive(Debug)]
pub enum ModuleError<'a> {
    ParserError(&'a ParseError),
    LexerError(&'a LexError),
    TypeError(&'a TypeError),
    ImportError(&'a ImportError),
}

pub fn module_declaration_not_global(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::NonGlobalModuleDeclaration,
        span,
    }
}

pub fn invalid_return(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::InvalidReturn,
        span,
    }
}

pub fn duplicate_constructor(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::DuplicateConstructor,
        span,
    }
}

pub fn empty_path_list(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::EmptyPathList,
        span,
    }
}

pub fn unknown_file_type(path_buf: PathBuf) -> ImportError {
    ImportError {
        _type: ImportErrorType::UnknownFileType { path_buf },
        span: None,
    }
}

pub fn error_reading_entry_file(error: std::io::Error) -> ImportError {
    ImportError {
        _type: ImportErrorType::ErrorReadingEntry(error),
        span: None,
    }
}

pub fn cannot_resolve_from_global_file() -> ImportError {
    ImportError {
        _type: ImportErrorType::ResolvingFromGlobalFile,
        span: None,
    }
}

pub fn io_error(error: std::io::Error) -> ImportError {
    ImportError {
        _type: ImportErrorType::VagueAccessError(error),
        span: None,
    }
}

pub fn cannot_find_module(module_name: String, span: ast::Span) -> ImportError {
    ImportError {
        _type: ImportErrorType::NonExistentModule(module_name),
        span: Some(span),
    }
}

pub fn self_import(target: &UseTarget) -> ImportError {
    ImportError {
        _type: ImportErrorType::SelfReferentialUse(target.name.name.clone()),
        span: Some(target.name.span),
    }
}

pub fn no_such_symbol_in_module(modulename: String, symbolname: ast::Identifier) -> ImportError {
    ImportError {
        _type: ImportErrorType::SymbolNotFound {
            modulename,
            symbolname: symbolname.name,
        },
        span: Some(symbolname.span),
    }
}

pub fn symbol_not_a_module(symbolname: ast::Identifier) -> ImportError {
    ImportError {
        _type: ImportErrorType::SymbolNotAModule {
            symbolname: symbolname.name,
        },
        span: Some(symbolname.span),
    }
}

pub fn private_symbol_leak(modulename: String, symbolname: ast::Identifier) -> ImportError {
    ImportError {
        _type: ImportErrorType::UsingPrivateSymbol {
            modulename,
            symbolname: symbolname.name,
        },
        span: Some(symbolname.span),
    }
}

pub fn public_in_non_global_scope(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::PublicAccessInNonGlobalScope,
        span,
    }
}

pub fn nameless_module() -> ImportError {
    ImportError {
        _type: ImportErrorType::NamelessModule,
        span: Some(ast::Span::default()),
    }
}

pub fn non_global_use(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::UseImportInNonGlobalScope,
        span,
    }
}

pub fn already_declared_in_scope(name: String, span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::AlreadyDeclaredInScope { name },
        span,
    }
}

pub fn use_before_declare(name: String, span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UseBeforeDeclare { name },
        span,
    }
}

pub fn this_outside_method(span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::ThisOutsideMethod,
        span,
    }
}

pub fn duplicate_property(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateModelProperty {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn duplicate_generic_parameter(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateGenericParameter {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn duplicate_parameter_names(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateParameterName {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn required_parameter_after_optional(span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::RequiredAfterOptional,
        span,
    }
}

pub fn empty_enum_tag(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::EmptyEnumTag,
        span,
    }
}

pub fn duplicate_enum_variant(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateEnumVariant {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn continue_outside_loop(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::ContinueOutsideLoop,
        span,
    }
}

pub fn break_outside_loop(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::BreakOutsideLoop,
        span,
    }
}

pub fn mismatched_file_and_module_name(
    module_name: &str,
    file_name: &str,
    module_ident_span: ast::Span,
) -> ImportError {
    ImportError {
        _type: ImportErrorType::MismatchInName {
            module_name: module_name.to_string(),
            file_name: file_name.to_string(),
        },
        span: Some(module_ident_span),
    }
}

pub fn not_callable(caller: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NotCallable { caller },
        span,
    }
}

pub fn illegal_model_call(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::IllegalModelCall { name },
        span,
    }
}

pub fn mismatched_function_args(
    span: ast::Span,
    expected: usize,
    found: usize,
    least_required: Option<usize>,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedFunctionArgs {
            expected,
            found,
            least_required,
        },
        span,
    }
}

pub fn missing_intrinsic(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MissingIntrinsic { name },
        span,
    }
}

pub fn invalid_index_subject(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidIndexSubject { name },
        span,
    }
}

pub fn model_not_constructable(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ModelNotConstructable { name },
        span,
    }
}

pub fn calling_new_on_identifier(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NewOnIdentifier { name },
        span,
    }
}

pub fn infinite_type(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InfiniteType,
        span,
    }
}

pub fn non_boolean_logic(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NonBooleanLogic { name },
        span,
    }
}

pub fn invalid_assignment_target(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidAssignmentTarget,
        span,
    }
}

pub fn mutating_method(owner: String, name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MutatingMethod { owner, name },
        span,
    }
}

pub fn assigning_to_reference(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AssigningToReference,
        span,
    }
}

pub fn separate_if_types(span: ast::Span, first: String, second: String) -> TypeError {
    TypeError {
        _type: TypeErrorType::SeparateIfTypes { first, second },
        span,
    }
}

pub fn void_assignment(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::VoidAssignment,
        span,
    }
}

pub fn partial_type_assignment(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::PartialTypeAssigmentIf,
        span,
    }
}
