use std::path::PathBuf;

#[derive(Debug)]
pub struct ResolveError {
    pub _type: ResolveErrorType,
    pub span: Option<whirl_ast::Span>,
}

#[derive(Debug)]
pub enum ResolveErrorType {
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
    MismatchedModuleCasing(String),
    UsingPrivateSymbol(String),
    UsingUnreachableModule(String),
    ClashWithInternalSymbol(String),
    ResolvingFromGlobalFile,
}
