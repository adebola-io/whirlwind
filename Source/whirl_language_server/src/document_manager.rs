use std::sync::RwLock;

use tower_lsp::{
    lsp_types::{
        DidOpenTextDocumentParams, Hover, HoverContents, HoverParams, MessageType, Position, Url,
    },
    Client,
};
use whirl_ast::{FunctionSignature, Statement};
use whirl_parser::Module;

use crate::hover_info::HoverInfo;

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
    pub fn get_hover_info(&self, params: HoverParams) -> tower_lsp::jsonrpc::Result<Option<Hover>> {
        let documents = self.documents.read().unwrap();
        let document = documents
            .iter()
            .find(|d| d.uri == params.text_document_position_params.text_document.uri);
        match document {
            Some(document) => {
                let hover_info =
                    document.get_hover_for_position(params.text_document_position_params.position);
                match hover_info {
                    Some(hover_info) => hover_info.into(),
                    None => Ok(None),
                }
            }
            None => Ok(None),
        }
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
        let position = [position.line, position.character];
        let mut i = 0;
        while i < self.module.statements.len() {
            let statement = &self.module.statements[i];
            if statement.span().contains(position) {
                match statement {
                    Statement::TestDeclaration => todo!(),
                    Statement::UseDeclaration => todo!(),
                    Statement::VariableDeclaration => todo!(),
                    Statement::ConstantDeclaration => todo!(),
                    Statement::ClassDeclaration => todo!(),
                    Statement::FunctionDeclaration(f) => {
                        // Hovering over the function name.
                        if f.signature.lock().unwrap().name.span.contains(position) {
                            return Some(HoverInfo {
                                contents: HoverContents::Array(vec![]),
                            });
                        }
                        return None;
                    }
                    Statement::RecordDeclaration => todo!(),
                    Statement::TraitDeclaration => todo!(),
                    Statement::EnumDeclaration => todo!(),
                    Statement::TypeDeclaration => todo!(),
                    Statement::PublicDeclaration(_) => todo!(),
                    Statement::WhileStatement => todo!(),
                    Statement::ForStatement => todo!(),
                    Statement::ExpressionStatement => todo!(),
                }
            }
        }
        return None;
    }
}
