use tower_lsp::lsp_types::TextDocumentContentChangeEvent;
use whirl_analyzer::Module;
use whirl_ast::Span;

pub struct ChangeHandler<'a> {
    pub module: &'a mut Module,
}

impl<'a> ChangeHandler<'a> {
    pub fn from_module(module: &'a mut Module) -> Self {
        ChangeHandler { module }
    }

    pub fn update(&self, changes: Vec<TextDocumentContentChangeEvent>) {
        for change in changes {
            let range = match change.range {
                Some(range) => range,
                None => continue,
            };
            let span = Span::from([
                range.start.line + 1,
                range.start.character,
                range.end.line + 1,
                range.end.character,
            ]);
            self.run_update(span, change.text);
        }
    }

    /// Change characteristics based on text update.
    pub fn run_update(&self, _span: Span, _text: String) {}
}
