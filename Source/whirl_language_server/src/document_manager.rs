use std::sync::RwLock;

use tower_lsp::lsp_types::{DidOpenTextDocumentParams, HoverParams, Position, Url};
use whirl_ast::ASTVisitor;
use whirl_parser::Module;

use crate::hover::{HoverFinder, HoverInfo};

#[derive(Debug)]
pub struct DocumentManager {
    documents: RwLock<Vec<WhirlDocument>>,
}

impl DocumentManager {
    pub fn new() -> Self {
        DocumentManager {
            documents: RwLock::new(vec![]),
        }
    }
    /// Add a new document to be tracked.
    pub fn add_document(&self, params: DidOpenTextDocumentParams) {
        let file = WhirlDocument {
            uri: params.text_document.uri,
            module: Module::from(params.text_document.text.as_str()),
        };
        self.documents.write().unwrap().push(file);
    }
    /// Hover over support.
    pub fn get_hover_info(&self, params: HoverParams) -> Option<HoverInfo> {
        let params = params.text_document_position_params;
        let documents = self.documents.read().unwrap();
        documents
            .iter()
            .find(|d| d.uri == params.text_document.uri)
            .map(|document| document.get_hover_for_position(params.position))
            .flatten()
    }
    /// Checks if a uri is already being tracked.
    pub fn has(&self, uri: &Url) -> bool {
        for file in self.documents.write().unwrap().iter() {
            if &file.uri == uri {
                return true;
            }
        }
        false
    }
}

#[derive(Debug)]
pub struct WhirlDocument {
    uri: Url,
    module: Module,
}

impl WhirlDocument {
    fn get_hover_for_position(&self, position: Position) -> Option<HoverInfo> {
        let position = [position.line + 1, position.character];
        let hover_finder = HoverFinder::with_scope_manager(&self.module.scope_manager);
        for statement in &self.module.statements {
            let hover_info = hover_finder.visit_statement(statement, &position);
            if hover_info.is_some() {
                return hover_info;
            }
        }
        return None;
    }
}
