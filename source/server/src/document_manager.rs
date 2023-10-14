use std::{path::PathBuf, sync::RwLock};

use crate::{
    diagnostic::to_diagnostic,
    error::DocumentError,
    hover::{HoverFinder, HoverInfo},
    message_store::MessageStore,
};
use analyzer::{FullProgramContext, Module, ModuleGraph};
use ast::ASTVisitorNoArgs;
use tower_lsp::lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    FullDocumentDiagnosticReport, HoverParams, Position, RelatedFullDocumentDiagnosticReport, Url,
};
use utils::get_parent_dir;

// Adds an error message to a message store and immediately returns the message store.
macro_rules! log_error {
    ($messages: expr, $($arg:tt)*) => {{
        $messages.error((format!($($arg)*)));
        return $messages;
    }};
}

#[derive(Debug)]
pub struct DocumentManager {
    pub contexts: RwLock<Vec<FullProgramContext>>,
}

impl DocumentManager {
    pub fn new() -> Self {
        let manager = DocumentManager {
            contexts: RwLock::new(vec![]),
        };
        manager
    }
    /// Add a new document to be tracked.
    pub fn add_document(&self, params: DidOpenTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();

        let mut contexts = self.contexts.write().unwrap();
        let uri = params.text_document.uri;
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path_buf) => path_buf,
            Err(err) => {
                log_error!(
                    msgs,
                    "Could not convert URI to an absolute file path. ERROR: {err:?}"
                )
            }
        };
        let parent_folder = match get_parent_dir(&path_buf) {
            Some(path) => path,
            None => {
                log_error!(msgs, "Could not determine parent folder for {path_buf:?}.")
            }
        };

        // Containing folder is already part of a context.
        for context in contexts.iter_mut() {
            if context.contains_folder(parent_folder) {
                match Module::from_path(path_buf, context.typed_modules.len()) {
                    Ok(module) => {
                        let path = module.module_path.as_ref().unwrap();
                        let message = match &module.name {
                            Some(name) => {
                                format!("Adding module {name} at path {:?}", path)
                            }
                            None => {
                                format!("Adding anonymous module at path {:?}", path)
                            }
                        };
                        msgs.inform(message);
                        context.add_module(module);
                        msgs.inform("Module added.");
                    }
                    // If module cannot be added, store error in the root file.
                    Err(error) => {
                        msgs.inform(format!("Error creating module from path, {error:?}"));
                        context.add_import_error(error);
                    }
                }
                return msgs;
            }
        }

        // No context has the file. Add a new context.
        let mut context = FullProgramContext::new(true);
        let mut root_folder = parent_folder;

        // Look 5 levels above to try to find the root of the project.
        for _ in 0..5 {
            let children: Vec<_> = match root_folder.read_dir() {
                Ok(rdir) => rdir,
                Err(error) => log_error!(msgs, "Error reading directory: {error:?}"),
            }
            .filter_map(|entry| entry.ok())
            .collect();
            // Find Main source module.
            let main_module = children
                .iter()
                .filter_map(|child| {
                    let path = child.path();
                    match path.is_file() && path.extension().is_some_and(|ext| ext == "wrl") {
                        true => Some(path),
                        false => None,
                    }
                })
                .filter_map(|file| Module::from_path(file, 0).ok())
                .find(|module| {
                    module
                        .name
                        .as_ref()
                        .is_some_and(|name| name == "Main" || name == "Lib")
                });
            if main_module.is_none() {
                root_folder = match get_parent_dir(&root_folder) {
                    Some(path) => path,
                    None => {
                        log_error!(msgs, "Could not determine parent folder during upwards traversal for {root_folder:?}.")
                    }
                };
                continue;
            }
            // Check again to see if there is already a graph with this module.
            for context in contexts.iter_mut() {
                if context.contains_folder(root_folder) {
                    match Module::from_path(path_buf, context.typed_modules.len()) {
                        Ok(module) => {
                            context.add_module(module);
                        }
                        Err(error) => context.add_import_error(error),
                    }
                    return msgs;
                }
            }
            let module = main_module.unwrap();
            msgs.inform(format!("Adding main module {:?}...", module.module_path));
            if let Some(()) = context.add_module(module) {
                msgs.inform("Main module added.")
            }
            // Todo: read whirlwind.yaml to find source module instead.
            break;
        }
        // Root not found, skip project altogether.
        if context.is_empty() {
            log_error!(msgs, "Could not determine main module for project.")
        }
        contexts.push(context);

        msgs
    }

    /// Hover over support.
    pub fn get_hover_info(&self, _params: HoverParams) -> Option<HoverInfo> {
        // let uri = params.text_document_position_params.text_document.uri;
        // let path_buf = uri_to_absolute_path(uri).ok()?;
        // let graphs = self.graphs.read().unwrap();
        // let main_graph = graphs.iter().find(|graph| graph.contains_file(&path_buf))?;
        // let module = main_graph.get_module_at_path(&path_buf)?;

        // get_hover_for_position(
        //     module,
        //     main_graph,
        //     params.text_document_position_params.position,
        // )
        None
    }
    /// Checks if a uri is already being tracked.
    pub fn has(&self, uri: Url) -> bool {
        let contexts = self.contexts.read().unwrap();
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path_buf) => path_buf,
            Err(_) => return false,
        };

        for context in contexts.iter() {
            if context.contains_file(&path_buf) {
                return true;
            }
        }
        return false;
    }

    pub fn completion(&self, _params: CompletionParams) -> Option<CompletionResponse> {
        // let graphs = self.graphs.read().unwrap();

        // let path = uri_to_absolute_path(params.text_document_position.text_document.uri).ok()?;
        // let graph = graphs.iter().find(|graph| graph.contains_file(&path))?;
        // let module = graph.get_module_at_path(&path)?;

        // let position = params.text_document_position.position;
        // let completion_context = params.context?;

        // let position = [position.line + 1, position.character + 1];
        // let statement = module
        //     .statements()
        //     .map(|statement| statement.closest_nodes_to(Span::at(position)))
        //     .flatten()
        //     .next()?;
        // let label = match statement {
        //     ast::Statement::UseDeclaration(u) => format!("Use"),
        //     _ => format!("Else"),
        // };
        // Some(CompletionResponse::Array(vec![CompletionItem {
        //     label,
        //     ..Default::default()
        // }]))
        None
    }

    /// Handles didChange event.
    pub fn handle_change(&self, mut params: DidChangeTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();
        msgs.inform("Handling text update...");

        let uri = params.text_document.uri;
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path) => path,
            Err(err) => log_error!(
                msgs,
                "Error converting URI to absolute path duting change: {err:?}"
            ),
        };
        let mut contexts = self.contexts.write().unwrap();
        let context = contexts
            .iter_mut()
            .find(|context| context.contains_file(&path_buf));

        let context = match context {
            Some(context) => context,
            None => log_error!(
                msgs,
                "Could not find module with this file, and therefore no changes are handled."
            ),
        };
        let last = params.content_changes.len() - 1;
        let most_current = std::mem::take(&mut params.content_changes[last].text);
        msgs.inform(format!("Refreshing module {path_buf:?}"));
        if let None = context.refresh_module_with_text(&path_buf, most_current) {
            log_error!(msgs, "Something went wrong while refreshing user text.")
        };

        msgs
    }
    /// Get diagnostics.
    pub fn get_diagnostics(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Option<DocumentDiagnosticReport> {
        let uri = params.text_document.uri;
        let path_buf = uri_to_absolute_path(uri).ok()?;

        let contexts = self.contexts.read().unwrap();
        let context = contexts
            .iter()
            .find(|context| context.contains_file(&path_buf))?;

        let path_idx = context.get_module_at_path(&path_buf)?.path;
        Some({
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: context
                        .errors
                        .iter()
                        .filter(|error| error.offending_file == path_idx)
                        .map(|p| to_diagnostic(p))
                        .collect::<Vec<Diagnostic>>(),
                },
            })
        })
    }
}

/// Traverse through the document and pinpoint the position of the hover.
fn _get_hover_for_position(
    module: &Module,
    graph: &ModuleGraph,
    pos: Position,
) -> Option<HoverInfo> {
    // Editor ranges are zero-based, for some reason.
    let pos = [pos.line + 1, pos.character + 1];
    let hover_finder = HoverFinder::new(module, graph, pos);
    for statement in module.statements() {
        let hover_info = hover_finder.statement(statement);
        if hover_info.is_some() {
            return hover_info;
        }
    }
    return None;
}

/// Convert a uri to an absolute path.
pub fn uri_to_absolute_path(uri: Url) -> Result<PathBuf, DocumentError> {
    Ok(uri
        .to_file_path()
        .or_else(|_| Err(DocumentError::CouldNotConvert(uri)))?
        .canonicalize()?)
}
