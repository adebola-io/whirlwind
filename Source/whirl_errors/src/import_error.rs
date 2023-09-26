use std::path::PathBuf;

#[derive(Debug)]
pub struct ImportError {
    pub _type: ImportErrorType,
    pub span: Option<whirl_ast::Span>,
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
    NamelessModule,
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
