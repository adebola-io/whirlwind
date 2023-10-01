mod context_error;
mod import_error;
mod lex_error;
mod parse_error;
mod type_error;

use std::path::PathBuf;

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

pub fn module_declaration_not_global(span: whirl_ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::NonGlobalModuleDeclaration,
        span,
    }
}

pub fn invalid_return(span: whirl_ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::InvalidReturn,
        span,
    }
}

pub fn duplicate_constructor(span: whirl_ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::DuplicateConstructor,
        span,
    }
}

pub fn empty_path_list(span: whirl_ast::Span) -> ParseError {
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

pub fn cannot_find_module(module_name: String, span: whirl_ast::Span) -> ImportError {
    ImportError {
        _type: ImportErrorType::NonExistentModule(module_name),
        span: Some(span),
    }
}

pub fn self_import(name: String, span: whirl_ast::Span) -> ImportError {
    ImportError {
        _type: ImportErrorType::SelfReferentialUse(name),
        span: Some(span),
    }
}

pub fn no_such_symbol_in_module(
    modulename: String,
    symbolname: whirl_ast::Identifier,
) -> ImportError {
    ImportError {
        _type: ImportErrorType::SymbolNotFound {
            modulename,
            symbolname: symbolname.name,
        },
        span: Some(symbolname.span),
    }
}

pub fn symbol_not_a_module(symbolname: whirl_ast::Identifier) -> ImportError {
    ImportError {
        _type: ImportErrorType::SymbolNotAModule {
            symbolname: symbolname.name,
        },
        span: Some(symbolname.span),
    }
}

pub fn private_symbol_leak(modulename: String, symbolname: whirl_ast::Identifier) -> ImportError {
    ImportError {
        _type: ImportErrorType::UsingPrivateSymbol {
            modulename,
            symbolname: symbolname.name,
        },
        span: Some(symbolname.span),
    }
}

pub fn public_in_non_global_scope(span: whirl_ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::PublicAccessInNonGlobalScope,
        span,
    }
}

pub fn nameless_module() -> ImportError {
    ImportError {
        _type: ImportErrorType::NamelessModule,
        span: Some(whirl_ast::Span::default()),
    }
}

pub fn non_global_use(span: whirl_ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::UseImportInNonGlobalScope,
        span,
    }
}

pub fn already_declared_in_scope(name: String, span: whirl_ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::AlreadyDeclaredInScope { name },
        span,
    }
}

pub fn use_before_declare(name: String, span: whirl_ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UseBeforeDeclare { name },
        span,
    }
}

pub fn this_outside_method(span: whirl_ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::ThisOutsideMethod,
        span,
    }
}
