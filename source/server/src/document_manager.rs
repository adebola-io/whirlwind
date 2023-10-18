use std::{path::PathBuf, sync::RwLock};

use crate::{
    diagnostic::to_diagnostic,
    error::DocumentError,
    hover::{HoverFinder, HoverInfo},
    message_store::{MessageStore, WithMessages},
};
use analyzer::{Module, Standpoint, TypedVisitorNoArgs, CORE_LIBRARY_PATH};
use tower_lsp::lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    FullDocumentDiagnosticReport, HoverParams, RelatedFullDocumentDiagnosticReport, Url,
};
use utils::get_parent_dir;

/// Adds an error message to a message store and immediately returns the message store.
macro_rules! log_error {
    ($messages: expr, $($arg:tt)*) => {{
        $messages.error((format!($($arg)*)));
        return $messages;
    }};
}

#[derive(Debug)]
pub struct DocumentManager {
    pub contexts: RwLock<Vec<Standpoint>>,
    pub corelib_path: Option<PathBuf>,
}

impl DocumentManager {
    pub fn new() -> Self {
        DocumentManager {
            contexts: RwLock::new(vec![]),
            corelib_path: Some(PathBuf::from(CORE_LIBRARY_PATH)),
        }
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
                match Module::from_path(path_buf) {
                    Ok(module) => {
                        let path = module.module_path.as_ref().unwrap();
                        let message = match &module.name {
                            Some(name) => {
                                format!("Folder already analyzed. Adding module {name} at path {path:?}")
                            }
                            None => {
                                format!("Folder already analyzed. Adding anonymous module at path {path:?}")
                            }
                        };
                        msgs.inform(message);
                        context.add_module(module);
                        msgs.inform(format!(
                            "Module added. {} modules in standpoint.",
                            context.module_map.len()
                        ));
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
        msgs.inform(format!("Creating new module context for {path_buf:?}"));
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
                .filter_map(|file| Module::from_path(file).ok())
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
            // Check again to see if there is already a graph with this folder.
            for context in contexts.iter_mut() {
                if context.contains_folder(root_folder) {
                    msgs.inform(format!("Found the context for {path_buf:?}"));
                    match Module::from_path(path_buf) {
                        Ok(module) => {
                            msgs.inform(format!("Module added successfully."));
                            context.add_module(module);
                        }
                        Err(error) => context.add_import_error(error),
                    }
                    return msgs;
                }
            }
            // Start at main module.
            let module = main_module.unwrap();
            let mut standpoint = Standpoint::new(true, self.corelib_path.clone());
            msgs.inform(format!(
                "New context created with {} modules. The core library path is {:?}.",
                standpoint.module_map.len(),
                standpoint
                    .corelib_path
                    .and_then(|path_idx| standpoint.module_map.get(path_idx))
                    .map(|module| &module.path_buf)
            ));
            msgs.inform(format!("Adding main module {:?}...", module.module_path));
            if let Some(_) = standpoint.add_module(module) {
                // now add the current module.
                match Module::from_path(path_buf) {
                    Ok(current_module) => match standpoint.add_module(current_module) {
                        Some(_) => msgs.inform("Module added successfully."),
                        None => log_error!(
                            msgs,
                            "Could not add this module to fresh standpoint. Skipping altogether.."
                        ),
                    },
                    Err(error) => msgs.inform(format!("Error creating current module: {error:?}")),
                }
            } else {
                // Root not found, skip project altogether.
                log_error!(msgs, "Could not determine main module for project.")
            }
            contexts.push(standpoint);
            // Todo: read whirlwind.yaml to find source module instead.
            break;
        }
        msgs
    }

    /// Hover over support.
    pub fn get_hover_info(&self, params: HoverParams) -> WithMessages<Option<HoverInfo>> {
        let mut messages = MessageStore::new();
        let uri = params.text_document_position_params.text_document.uri;
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(p) => p,
            Err(error) => {
                messages.error(format!(
                    "Could not convert uri for hover in file. ERROR: {error:?}"
                ));
                return (messages, None);
            }
        };
        let contexts = self.contexts.read().unwrap();
        let main_context = match contexts
            .iter()
            .find(|context| context.contains_file(&path_buf))
        {
            Some(context) => context,
            None => {
                messages.error(format!("Could not find context for {path_buf:?}"));
                return (messages, None);
            }
        };
        let module = match main_context.get_module_at_path(&path_buf) {
            Some(m) => m,
            None => {
                messages.error(format!(
                    "Could not find module with path {path_buf:?} in context."
                ));
                return (messages, None);
            }
        };
        let pos = params.text_document_position_params.position;
        // Editor ranges are zero-based, for some reason.
        let pos = [pos.line + 1, pos.character + 1];
        let _hover_finder = HoverFinder::new(module, main_context, pos, messages);
        for statement in module.statements.iter() {
            if let Some(hover) = _hover_finder.statement(statement) {
                return (_hover_finder.message_store.take(), Some(hover));
            }
        }
        return (_hover_finder.message_store.take(), None);
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
                "Error converting URI to absolute path during change: {err:?}"
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
        msgs.inform(format!("Errors: {:#?}", context.errors));
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

        let path_idx = context
            .module_map
            .paths()
            .find(|tuple| tuple.1.path_buf == path_buf)?
            .0;
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

/// Convert a uri to an absolute path.
pub fn uri_to_absolute_path(uri: Url) -> Result<PathBuf, DocumentError> {
    Ok(uri
        .to_file_path()
        .or_else(|_| Err(DocumentError::CouldNotConvert(uri)))?
        .canonicalize()?)
}
