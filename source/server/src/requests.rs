use tower_lsp::lsp_types::request::Request;

/// A file is being saved in the editor.
pub struct SavingStart();

impl Request for SavingStart {
    type Params = ();
    type Result = ();
    const METHOD: &'static str = "saving-start";
}

/// A file is being saved in the editor.
pub struct SavingEnd();

impl Request for SavingEnd {
    type Params = ();
    type Result = ();
    const METHOD: &'static str = "saving-end";
}
