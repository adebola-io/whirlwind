use crate::Span;

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}
