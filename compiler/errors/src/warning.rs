use ast::Span;

#[derive(PartialEq, Debug)]
pub struct Warning {
    pub span: Span,
    pub warning_type: WarningType,
}

#[derive(PartialEq, Debug)]
pub enum WarningType {
    UnusedImportSymbol(String),
    UnusedModelSymbol(String),
}
