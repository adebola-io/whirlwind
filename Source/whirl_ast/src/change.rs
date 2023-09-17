use crate::Span;

/// An edit to the source of a module.
pub struct Change {
    pub span: Span,
    pub new_text: String,
}
