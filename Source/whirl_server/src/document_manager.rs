use std::sync::RwLock;

use tower_lsp::lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentDiagnosticParams,
    DocumentDiagnosticReport, FullDocumentDiagnosticReport, HoverParams, Position,
    RelatedFullDocumentDiagnosticReport, TextDocumentContentChangeEvent, Url,
};
use whirl_analyzer::Module;
use whirl_ast::ASTVisitor;

use crate::{
    diagnostic::to_diagnostic,
    did_change::ChangeHandler,
    hover::{HoverFinder, HoverInfo},
};

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
            version: 0,
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
    /// Handles didChange event
    pub fn handle_change(&self, params: DidChangeTextDocumentParams) {
        let mut documents = self.documents.write().unwrap();
        let document = documents
            .iter_mut()
            .find(|d| d.uri == params.text_document.uri);
        if document.is_none() {
            return;
        }
        let document = document.unwrap();
        document.refresh(params.text_document.version, params.content_changes);
    }
    /// Get diagnostics.
    pub fn get_diagnostics(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Option<DocumentDiagnosticReport> {
        let mut documents = self.documents.write().unwrap();
        let document = documents
            .iter_mut()
            .find(|d| d.uri == params.text_document.uri);
        document.map(|doc| {
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: doc
                        .module
                        .program_errors
                        .iter()
                        .map(|p| to_diagnostic(&doc.module.scope_manager, p))
                        .collect::<Vec<Diagnostic>>(),
                },
            })
        })
    }
}

#[derive(Debug)]
pub struct WhirlDocument {
    uri: Url,
    version: usize,
    module: Module,
}

impl WhirlDocument {
    /// Traverse through the document and pinpoint the position of the hover.
    fn get_hover_for_position(&self, position: Position) -> Option<HoverInfo> {
        // Editor ranges are zero-based, for some reason.
        let position = [position.line + 1, position.character + 1];
        let hover_finder = HoverFinder::with_scope_manager(&self.module.scope_manager);
        for statement in &self.module.statements {
            let hover_info = hover_finder.visit_statement(statement, &position);
            if hover_info.is_some() {
                return hover_info;
            }
        }
        return None;
    }
    /// Refresh a document with a set of changes to the text.
    fn refresh(&mut self, version: i32, changes: Vec<TextDocumentContentChangeEvent>) {
        self.version = version as usize;
        let handler = ChangeHandler::from_module(&mut self.module);
        handler.update(changes);
    }
}
