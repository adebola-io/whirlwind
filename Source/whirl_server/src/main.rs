mod diagnostic;
mod error;

mod document_manager;
mod hover;

use document_manager::{uri_to_absolute_path, DocumentManager};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    docs: DocumentManager,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: None,
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: Some(true),
                        },
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let current_graph_count = self.docs.graphs.read().unwrap().len();
        if self.docs.has(uri.clone()) {
            return;
        }
        let path = uri_to_absolute_path(uri);
        if let Err(err) = self.docs.add_document(params) {
            self.log_message(&format!("Error Adding file: {:?}, ----  {:?}", path, err))
                .await
        }
        let new_graph_count = self.docs.graphs.read().unwrap().len();
        if new_graph_count > current_graph_count {
            self.log_message(&format!(
                "Added new graph for project containing {:?}",
                path
            ))
            .await
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.docs.handle_change(params);
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        // self.client
        //     .log_message(MessageType::INFO, "Hovering...")
        //     .await;
        Ok(self.docs.get_hover_info(params).map(|h| h.into()))
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let report = self.docs.get_diagnostics(params);
        Ok(DocumentDiagnosticReportResult::Report(
            report.unwrap_or_else(|| {
                DocumentDiagnosticReport::Unchanged(RelatedUnchangedDocumentDiagnosticReport {
                    related_documents: None,
                    unchanged_document_diagnostic_report: UnchangedDocumentDiagnosticReport {
                        result_id: format!(""),
                    },
                })
            }),
        ))
    }
}

impl Backend {
    async fn log_message(&self, message: &str) {
        self.client.log_message(MessageType::INFO, message).await
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        docs: DocumentManager::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
