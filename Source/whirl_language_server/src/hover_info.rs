use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{Hover, HoverContents},
};

pub struct HoverInfo {
    pub contents: HoverContents,
}

impl From<HoverInfo> for Result<Option<Hover>> {
    fn from(value: HoverInfo) -> Self {
        Ok(Some(Hover {
            contents: value.contents,
            range: None,
        }))
    }
}
