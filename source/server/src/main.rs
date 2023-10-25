mod diagnostic;
mod error;

mod document_manager;
mod hover;
mod message_store;

use document_manager::DocumentManager;
use message_store::MessageStore;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::{GotoDeclarationParams, GotoDeclarationResponse};
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
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: None,
                        inter_file_dependencies: true,
                        workspace_diagnostics: true,
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

    async fn goto_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> Result<Option<GotoDeclarationResponse>> {
        let (messages, declaration_response) = self.docs.get_declaration(params);
        self.log_all(messages).await;
        Ok(declaration_response)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let (messages, declaration_response) = self.docs.get_declaration(params);
        self.log_all(messages).await;
        Ok(declaration_response)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let (messages, hoverinfo) = self.docs.get_hover_info(params);
        self.log_all(messages).await;
        Ok(hoverinfo.map(|h| h.into()))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let (messages, locations) = self.docs.get_references(params);
        self.log_all(messages).await;
        Ok(locations)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let (messages, result) = self.docs.rename(params);
        self.log_all(messages).await;
        result
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

    async fn workspace_diagnostic(
        &self,
        params: WorkspaceDiagnosticParams,
    ) -> Result<WorkspaceDiagnosticReportResult> {
        let (messages, diagnostics) = self.docs.get_workspace_diagnostics(params);
        self.log_all(messages).await;
        Ok(diagnostics)
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
