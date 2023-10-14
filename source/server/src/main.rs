mod diagnostic;
mod error;

mod document_manager;
mod hover;
mod message_store;

use document_manager::DocumentManager;
use message_store::MessageStore;
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
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![format!("."), format!("use ")]),
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(true),
                    },
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                }),
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

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        if !self.docs.has(params.text_document.uri.clone()) {
            let messages = self.docs.add_document(params);
            self.log_all(messages).await;
        } else {
            self.log_message("Opened analyzed file.").await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let messages = self.docs.handle_change(params);
        self.log_all(messages).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        Ok(self.docs.get_hover_info(params).map(|h| h.into()))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(self.docs.completion(params))
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
    async fn log_message<T: std::fmt::Debug>(&self, message: T) {
        self.client
            .log_message(MessageType::INFO, format!("{:?}", message))
            .await
    }
    async fn log_all(&self, message_store: MessageStore) {
        for message in message_store {
            self.client.log_message(message.0, message.1).await
        }
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
