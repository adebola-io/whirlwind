use crate::Span;

/// An edit to the source of a module.
pub struct Change {
    pub span: Span,
    pub new_text: String,
}

impl Change {
    pub fn new(range_of_change: [u32; 4], new_text: &str) -> Change {
        Self {
            span: Span::from(range_of_change),
            new_text: new_text.to_owned(),
        }
    }
}
