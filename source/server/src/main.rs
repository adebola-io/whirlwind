mod completion;
mod diagnostic;
mod document_manager;
mod error;
mod folding_range;
mod hover;
mod message_store;
mod requests;

use analyzer::{Module, Standpoint};
use ast::unwrap_or_continue;
use document_manager::{uri_to_absolute_path, DocumentManager};
use message_store::MessageStore;
use requests::{SavingEnd, SavingStart};
use std::path::PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::{GotoDeclarationParams, GotoDeclarationResponse};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    _args: Vec<String>,
    docs: DocumentManager,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Initializes the server by setting by analyzing the workspace folders.
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let workspace_folders = params.workspace_folders;
        if let Some(folders) = workspace_folders {
            let mut standpoints = self.docs.standpoints.lock().unwrap();
            let mut standpoint = Standpoint::new(true, self.docs.corelib_path.clone());
            for folder in folders {
                let root_folder_path = unwrap_or_continue!(uri_to_absolute_path(folder.uri).ok());
                let children = unwrap_or_continue!(root_folder_path.read_dir().ok())
                    .filter_map(|entry| entry.ok());
                children.for_each(|entry| {
                    let file_path = entry.path();
                    if file_path.is_file()
                        && file_path
                            .extension()
                            .and_then(|ext| ext.to_str())
                            .map(|ext| ext == "wrl")
                            .is_some_and(|is_wrl_file| is_wrl_file)
                    {
                        if !standpoint.contains_file(&file_path) {
                            Module::from_path(file_path)
                                .ok()
                                .and_then(|module| standpoint.add_module(module));
                        }
                    }
                });
                standpoint.validate();
            }
            standpoints.push(standpoint);
        }
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: String::from("Whirlwind Language Server"),
                version: Some(String::from("0.0.0")),
            }),
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![format!("."), format!(" "), format!("&")]),
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
                inlay_hint_provider: Some(OneOf::Left(true)),
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
        self.log_message("Extension Name: Whirlwind Language Server")
            .await;
        self.log_message("Extension Version: 0.0.0").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if !self.docs.has(uri) {
            let messages = self.docs.add_document(params);
            self.log_all(messages).await;
        } else {
            self.docs.open_document(params);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let _ = self.client.send_request::<SavingStart>(()).await;
        let messages = self.docs.save_file(params);
        let _ = self.client.send_request::<SavingEnd>(()).await;
        self.log_all(messages).await;
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

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let (messages, inlay_response) = self.docs.get_hints(params);
        self.log_all(messages).await;
        Ok(inlay_response)
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
        let (messages, completion) = self.docs.completion(params);
        self.log_all(messages).await;
        Ok(completion)
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let (messages, ranges) = self.docs.get_folding_ranges(params);
        self.log_all(messages).await;
        Ok(ranges)
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
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
    async fn log_message<T: std::fmt::Display>(&self, message: T) {
        self.client
            .log_message(MessageType::INFO, format!("{}", message))
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
    std::env::set_var("RUST_BACKTRACE", "1");
    let _args: Vec<String> = std::env::args().collect();
    let core_path = _args.get(1).map(|arg| PathBuf::from(arg));
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        _args,
        docs: DocumentManager::new(core_path),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
