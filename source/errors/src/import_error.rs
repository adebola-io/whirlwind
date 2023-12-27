use std::path::PathBuf;

use ast::UseTarget;

#[derive(Debug, PartialEq)]
pub struct ImportError {
    pub _type: ImportErrorType,
    pub span: Option<ast::Span>,
}

#[derive(Debug)]
pub enum ImportErrorType {
    AmbiguousImport {
        modulename: String,
        offending_files: Vec<PathBuf>,
    },
    ErrorReadingEntry(std::io::Error),
    UnknownFileType {
        path_buf: PathBuf,
    },
    ErrorReadingModule {
        modulename: String,
    },
    VagueAccessError(std::io::Error),
    NonExistentModule(String),
    DuplicatedModuleNameInSameFolder(String),
    SelfReferentialUse(String),
    MismatchedModuleCasing {
        mistake: String,
        real: String,
    },
    UsingPrivateSymbol {
        modulename: String,
        symbolname: String,
    },
    UsingUnreachableModule(String),
    ClashWithInternalSymbol(String),
    ResolvingFromGlobalFile,
    SymbolNotFound {
        modulename: String,
        symbolname: String,
    },
    SymbolNotAModule {
        symbolname: String,
    },
}

impl PartialEq for ImportErrorType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::AmbiguousImport {
                    modulename: l_modulename,
                    offending_files: l_offending_files,
                },
                Self::AmbiguousImport {
                    modulename: r_modulename,
                    offending_files: r_offending_files,
                },
            ) => l_modulename == r_modulename && l_offending_files == r_offending_files,
            (Self::ErrorReadingEntry(l0), Self::ErrorReadingEntry(r0)) => l0.kind() == r0.kind(),
            (
                Self::UnknownFileType {
                    path_buf: l_path_buf,
                },
                Self::UnknownFileType {
                    path_buf: r_path_buf,
                },
            ) => l_path_buf == r_path_buf,
            (
                Self::ErrorReadingModule {
                    modulename: l_modulename,
                },
                Self::ErrorReadingModule {
                    modulename: r_modulename,
                },
            ) => l_modulename == r_modulename,
            (Self::VagueAccessError(l0), Self::VagueAccessError(r0)) => l0.kind() == r0.kind(),
            (Self::NonExistentModule(l0), Self::NonExistentModule(r0)) => l0 == r0,
            (
                Self::DuplicatedModuleNameInSameFolder(l0),
                Self::DuplicatedModuleNameInSameFolder(r0),
            ) => l0 == r0,
            (Self::SelfReferentialUse(l0), Self::SelfReferentialUse(r0)) => l0 == r0,
            (
                Self::MismatchedModuleCasing {
                    mistake: l_mistake,
                    real: l_real,
                },
                Self::MismatchedModuleCasing {
                    mistake: r_mistake,
                    real: r_real,
                },
            ) => l_mistake == r_mistake && l_real == r_real,
            (
                Self::UsingPrivateSymbol {
                    modulename: l_modulename,
                    symbolname: l_symbolname,
                },
                Self::UsingPrivateSymbol {
                    modulename: r_modulename,
                    symbolname: r_symbolname,
                },
            ) => l_modulename == r_modulename && l_symbolname == r_symbolname,
            (Self::UsingUnreachableModule(l0), Self::UsingUnreachableModule(r0)) => l0 == r0,
            (Self::ClashWithInternalSymbol(l0), Self::ClashWithInternalSymbol(r0)) => l0 == r0,
            (
                Self::SymbolNotFound {
                    modulename: l_modulename,
                    symbolname: l_symbolname,
                },
                Self::SymbolNotFound {
                    modulename: r_modulename,
                    symbolname: r_symbolname,
                },
            ) => l_modulename == r_modulename && l_symbolname == r_symbolname,
            (
                Self::SymbolNotAModule {
                    symbolname: l_symbolname,
                },
                Self::SymbolNotAModule {
                    symbolname: r_symbolname,
                },
            ) => l_symbolname == r_symbolname,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
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
