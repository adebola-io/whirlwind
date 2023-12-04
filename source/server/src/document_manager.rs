use crate::{
    completion::{sort_completions, CompletionFinder, DotCompletionType, Trigger},
    diagnostic::{error_to_diagnostic, to_range},
    error::DocumentError,
    hover::{HoverFinder, HoverInfo},
    message_store::{MessageStore, WithMessages},
};
use analyzer::{
    utils::symbol_to_type, Module, PathIndex, SemanticSymbol, SemanticSymbolDeclaration,
    SemanticSymbolKind, Standpoint, StandpointStatus, SymbolIndex, TypedModule, TypedVisitorNoArgs,
};
use ast::{is_keyword_or_operator, is_valid_identifier, is_valid_identifier_start, Span};
use printer::SymbolWriter;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};
use tower_lsp::{
    jsonrpc::Error,
    lsp_types::{
        request::{GotoDeclarationParams, GotoDeclarationResponse},
        CompletionContext, CompletionParams, CompletionResponse, Diagnostic,
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
        DocumentDiagnosticParams, DocumentDiagnosticReport, FullDocumentDiagnosticReport,
        HoverParams, InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Location, Position,
        ReferenceParams, RelatedFullDocumentDiagnosticReport, RenameParams, TextEdit, Url,
        WorkspaceDiagnosticParams, WorkspaceDiagnosticReport, WorkspaceDiagnosticReportResult,
        WorkspaceDocumentDiagnosticReport, WorkspaceEdit, WorkspaceFullDocumentDiagnosticReport,
    },
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
    pub standpoints: Arc<Mutex<Vec<Standpoint>>>,
    pub corelib_path: Option<PathBuf>,
    /// Boolean stopper to prevent unnecessary workspace diagnostic refreshes.
    was_updated: Arc<Mutex<bool>>,
    /// Cached workspace diagnostic report.
    diagnostic_report: Arc<Mutex<WorkspaceDiagnosticReportResult>>,
    /// Last opened standpoint module.
    last_opened: Arc<Mutex<Option<(usize, PathIndex)>>>,
}

impl DocumentManager {
    /// Creates a new document manager.
    pub fn new(corelib_path: Option<PathBuf>) -> Self {
        DocumentManager {
            standpoints: Arc::new(Mutex::new(vec![])),
            corelib_path,
            was_updated: Arc::new(Mutex::new(true)),
            diagnostic_report: Arc::new(Mutex::new(WorkspaceDiagnosticReportResult::Report(
                Default::default(),
            ))),
            last_opened: Arc::new(Mutex::new(None)),
        }
    }

    /// Handle the opening of a document/module.
    pub fn open_document(&self, params: DidOpenTextDocumentParams) {
        let mut standpoints = self.standpoints.lock().unwrap();
        let path_buf = match uri_to_absolute_path(params.text_document.uri.clone()) {
            Ok(path_buf) => path_buf,
            Err(_) => return,
        };
        for standpoint in standpoints.iter_mut() {
            if let Some(module) = standpoint.module_map.map_path_to_index(&path_buf) {
                standpoint.refresh_module(module, &params.text_document.text);
            }
        }
    }

    /// Sets the open module.
    pub fn remember(&self, uri: Url) -> MessageStore {
        let mut msgs = MessageStore::new();
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path_buf) => path_buf,
            Err(err) => {
                log_error!(
                    msgs,
                    "Could not convert URI to an absolute file path. ERROR: {err:?}"
                )
            }
        };
        let standpoints = self.standpoints.lock().unwrap();
        match self.get_cached(&standpoints) {
            Some((_, module)) => {
                if module.path_buf == path_buf {
                    // Document/module is already opened.
                    return msgs;
                }
                self.update_cached(&standpoints, path_buf, &mut msgs);
            }
            None => self.update_cached(&standpoints, path_buf, &mut msgs),
        };
        msgs
    }

    /// Handles a file save in the editor.
    pub fn save_file(&self, _params: DidSaveTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();
        msgs.inform("Saving file...");
        msgs
    }

    /// Update the cached module.
    fn update_cached(
        &self,
        standpoints: &Vec<Standpoint>,
        path_buf: PathBuf,
        msgs: &mut MessageStore,
    ) {
        let context = standpoints
            .iter()
            .enumerate()
            .find(|(_, context)| context.contains_file(&path_buf));
        let (index, standpoint) = match context {
            Some(context) => context,
            None => {
                msgs.error(format!(
                    "Could not find module with this file, and therefore no changes are handled."
                ));
                return;
            }
        };
        let module_idx = match standpoint.module_map.map_path_to_index(&path_buf) {
            Some(path_idx) => path_idx,
            None => {
                msgs.error("Could not retrieve cached module.");
                return;
            }
        };
        let mut last_opened = self.last_opened.lock().unwrap();
        *last_opened = Some((index, module_idx));
    }

    /// Return the cached module.
    fn get_cached<'a>(
        &self,
        standpoints: &'a Vec<Standpoint>,
    ) -> Option<(&'a Standpoint, &'a TypedModule)> {
        let last_opened = self.last_opened.lock().unwrap();
        let last_opened = last_opened.as_ref()?;
        let last_opened_standpoint = standpoints.get(last_opened.0)?;
        let last_opened_module = last_opened_standpoint.module_map.get(last_opened.1)?;
        Some((last_opened_standpoint, last_opened_module))
    }

    /// Return the cached module, mutably.
    fn get_cached_mut<'a>(
        &self,
        standpoints: &'a mut Vec<Standpoint>,
    ) -> Option<(&'a mut Standpoint, PathIndex)> {
        let last_opened = self.last_opened.lock().unwrap();
        let last_opened = last_opened.as_ref()?;
        let last_opened_standpoint = standpoints.get_mut(last_opened.0)?;
        let last_opened_module = last_opened_standpoint.module_map.get_mut(last_opened.1)?;
        let idx = last_opened_module.path_idx.clone();
        Some((last_opened_standpoint, idx))
    }

    /// Add a new document to be tracked.
    pub fn add_document(&self, params: DidOpenTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();
        let mut standpoints = self.standpoints.lock().unwrap();
        let uri = params.text_document.uri;
        let path_buf = match uri_to_absolute_path(uri.clone()) {
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
        for context in standpoints.iter_mut() {
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
                        msgs.inform(format!("{:?}", &module.module_path));
                        msgs.inform(format!("{:?}", &module.name));

                        msgs.inform(message);
                        match context.add_module(module) {
                            Some(p) => msgs.inform(format!(
                                "Module added at index {p:?}. {} modules in standpoint.",
                                context.module_map.len()
                            )),
                            None => msgs.inform("The module was not added. Something went wrong."),
                        }
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
            let main_module = match main_module {
                Some(module) => module,
                None => {
                    root_folder = match get_parent_dir(&root_folder) {
                        Some(path) => path,
                        None => {
                            log_error!(msgs, "Could not determine parent folder during upwards traversal for {root_folder:?}.")
                        }
                    };
                    continue;
                }
            };
            // Check again to see if there is already a graph with this parent folder.
            for context in standpoints.iter_mut() {
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
            let mut standpoint = Standpoint::new(true, self.corelib_path.clone());
            msgs.inform(format!(
                "New context created with {} modules. The core library path is {:?}. The prelude path is {:?}",
                standpoint.module_map.len(),
                standpoint
                    .corelib_path
                    .and_then(|path_idx| standpoint.module_map.get(path_idx))
                    .map(|module| &module.path_buf),
                standpoint
                    .prelude_path
                    .and_then(|path_idx| standpoint.module_map.get(path_idx))
                    .map(|module| &module.path_buf)
            ));

            msgs.inform(format!(
                "Adding main module {:?}...",
                main_module.module_path
            ));
            // Start at main module.
            if let Some(_) = standpoint.add_module(main_module) {
                // now add the current module. (if it was not already automatically added.)
                if !standpoint.contains_file(&path_buf) {
                    match Module::from_path(path_buf) {
                        Ok(current_module) => {
                            if let None = standpoint.add_module(current_module) {
                                log_error!(
                            msgs,
                            "Could not add this module to fresh standpoint. Skipping altogether.."
                        )
                            }
                        }
                        Err(error) => {
                            msgs.inform(format!("Error creating current module: {error:?}"));
                            standpoints.push(standpoint);
                            return msgs;
                        }
                    }
                };
                msgs.inform("Module added successfully.");
            } else {
                // Root not found, skip project altogether.
                log_error!(msgs, "Could not determine main module for project.")
            }
            standpoints.push(standpoint);
            // Todo: read whirlwind.yaml to find source module instead.
            break;
        }
        msgs
    }

    /// Hover over support.
    pub fn get_hover_info(&self, params: HoverParams) -> WithMessages<Option<HoverInfo>> {
        let mut messages = self.remember(params.text_document_position_params.text_document.uri);
        let time = std::time::Instant::now();
        let standpoints = self.standpoints.lock().unwrap();
        let (main_standpoint, module) = match self.get_cached(&standpoints) {
            Some(t) => t,
            None => {
                messages.error("Could not retrieve the cached module index");
                return (messages, None);
            }
        };
        let position = params.text_document_position_params.position;
        // Editor ranges are zero-based, for some reason.
        let pos = [position.line + 1, position.character + 1];
        let hover_finder = HoverFinder::new(module, main_standpoint, pos, messages);
        for statement in module.statements.iter() {
            if let Some(hover) = hover_finder.statement(statement) {
                let mut messages = hover_finder.message_store.take();
                messages.inform(format!("Retreived hover info in {:?}", time.elapsed()));
                return (messages, Some(hover));
            }
        }
        return (hover_finder.message_store.take(), None);
    }

    /// Checks if a uri is already being tracked.
    pub fn has(&self, uri: Url) -> bool {
        let contexts = self.standpoints.lock().unwrap();
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

    /// Gets completion response.
    /// TODO: This will obviously get sloooooww. Fix.
    pub fn completion(&self, params: CompletionParams) -> WithMessages<Option<CompletionResponse>> {
        let mut msgs = self.remember(params.text_document_position.text_document.uri);
        let standpoints = self.standpoints.lock().unwrap();
        let (standpoint, module) = match self.get_cached(&standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let position = params.text_document_position.position;
        // Editor ranges are zero-based, for some reason.
        let pos = [position.line + 1, position.character + 1];
        msgs.inform(format!("Gathering completions for {pos:?}..."));
        let time = std::time::Instant::now();
        let completion_finder =
            CompletionFinder::new(module, standpoint, pos, msgs, params.context);
        for (index, statement) in module.statements.iter().enumerate() {
            completion_finder.next_statement_is(index + 1, &module.statements);
            if let Some(completions) = completion_finder.statement(statement) {
                let mut messages = completion_finder.message_store.take();
                messages.inform(format!("Retreived completions in {:?}", time.elapsed()));
                return (messages, Some(completions));
            }
        }
        completion_finder
            .message_store
            .borrow_mut()
            .inform("Could not complete by traversal. Checking all symbols...");
        let symboltable = &standpoint.symbol_table;
        let trigger = match &completion_finder.context {
            Some(CompletionContext {
                trigger_character, ..
            }) => match trigger_character {
                Some(trigger) => match trigger.as_str() {
                    "use " => Trigger::UseImport,
                    "new " => Trigger::NewInstance,
                    "implements " => Trigger::Implements,
                    "." => Trigger::DotAccess,
                    "&" => Trigger::Ampersand,
                    " " => Trigger::WhiteSpace,
                    ": " => Trigger::TypeLabel,
                    _ => Trigger::None,
                },
                None => Trigger::None,
            },
            None => Trigger::None,
        };
        let trigger_is_dot = matches!(trigger, Trigger::DotAccess);
        let mut response = None;
        // Found a symbol that is directly before the completion context. Attempt to provide a completion for it.
        match trigger {
            Trigger::DotAccess => {
                let closest = get_closest_symbol(symboltable, module, pos, trigger_is_dot);
                if closest.is_some() {
                    let (index, symbol) = closest.unwrap();
                    let inferred_type = match symbol_to_type(symbol, index, symboltable) {
                        Ok(typ) => typ,
                        Err(_) => return (completion_finder.message_store.take(), None),
                    };
                    response =
                        completion_finder.complete_from_dot(inferred_type, DotCompletionType::Half);
                }
            }
            Trigger::UseImport => {
                // Client has typed "use " and is expecting a list of modules.
                // Suggests the list of modules in the current directory, along with the Core library.
                // NOTE: the "use " trigger is client dependent.
                let mut completions = vec![];
                let writer = SymbolWriter::new(standpoint);
                let current_module_path = &module.path_buf;
                response = (|| -> Option<CompletionResponse> {
                    let parent_directory = get_parent_dir(&current_module_path)?;
                    let directory_map = standpoint.directories.get(parent_directory)?;
                    for module_path_index in directory_map
                        .iter()
                        .map(|(_, idx)| idx)
                        .chain(standpoint.corelib_path.iter())
                    {
                        if *module_path_index == module.path_idx {
                            continue;
                        }
                        let module = standpoint.module_map.get(*module_path_index)?;
                        let symbol_idx = module.symbol_idx;
                        let symbol = standpoint.symbol_table.get(symbol_idx)?;
                        if let Some(completion) =
                            completion_finder.create_completion(&writer, symbol, symbol_idx, false)
                        {
                            completions.push(completion)
                        }
                    }
                    sort_completions(&mut completions);
                    return Some(CompletionResponse::Array(completions));
                })();
            }
            trigger => {
                // Attempt regular autocomplete.
                // Preventing completion in comments should be handled by the client.
                let closest = get_closest_symbol(symboltable, module, pos, trigger_is_dot);
                let mut completions = vec![];
                let writer = SymbolWriter::new(standpoint);
                for (index, symbol) in symboltable.symbols().filter(|(_, sym)| {
                    is_valid_complete_target(sym, module, standpoint, pos, closest, trigger)
                }) {
                    if let Some(completion) =
                        completion_finder.create_completion(&writer, symbol, index, true)
                    {
                        completions.push(completion);
                    }
                }
                sort_completions(&mut completions);
                response = Some(CompletionResponse::Array(completions));
            }
        }
        let mut messages = completion_finder.message_store.take();
        messages.inform(format!("Retreived completions in {:?}", time.elapsed()));
        return (messages, response);
    }

    /// Handles a change in the text of a module.
    pub fn handle_change(&self, mut params: DidChangeTextDocumentParams) -> MessageStore {
        let uri = params.text_document.uri.clone();
        let mut msgs = self.remember(uri);
        let mut standpoints = self.standpoints.lock().unwrap();
        let (standpoint, path_idx) = match self.get_cached_mut(&mut standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return msgs;
            }
        };
        let last = params.content_changes.len() - 1;
        let most_current = std::mem::take(&mut params.content_changes[last].text);
        let time = std::time::Instant::now();
        match standpoint.refresh_module(path_idx, &most_current) {
            Some(StandpointStatus::RefreshSuccessful) => {
                msgs.inform(format!("Document refreshed in {:?}", time.elapsed()));
                msgs.inform(format!("Symbols: {:?}", standpoint.symbol_table.len()));
            }
            _ => log_error!(msgs, "Something went wrong while refreshing user text."),
        };
        *self.was_updated.lock().unwrap() = true;
        // idk what to do here.
        // todo: set threshold according to size of standpoints.
        // if handling changes becomes too slow, the entire standpoint is refreshed.
        // All modules are removed and added again, and the current module is updated with the latest text.
        if time.elapsed() > std::time::Duration::from_millis(get_time_limit(&standpoint.module_map))
        {
            msgs.inform("Threshold crossed. Refreshing standpoint.");
            // The current module should not be part of the list of modules to re-add.
            let modules = standpoint
                .module_map
                .paths()
                .filter(|(idx, _)| *idx != path_idx)
                .map(|(_, typedmodule)| typedmodule.path_buf.clone())
                .collect::<Vec<_>>();
            *standpoint = Standpoint::new(true, self.corelib_path.clone());
            for module_path in modules {
                if standpoint
                    .module_map
                    .map_path_to_index(&module_path)
                    .is_none()
                {
                    match Module::from_path(module_path) {
                        Ok(module) => {
                            standpoint.add_module(module);
                        }
                        Err(error) => {
                            msgs.error(format!("Import Error: {error:?}"));
                            continue;
                        }
                    }
                }
            }
            // Add current module with current text.
            let uri = params.text_document.uri;
            let mut module = Module::from_text(&most_current);
            let path = uri_to_absolute_path(uri).ok();
            if path.is_none() {
                return msgs;
            }
            let path = path.unwrap();
            module.module_path = Some(path);
            standpoint.add_module(module);
        };
        msgs
    }

    /// Returns the first declaration of a symbol.
    pub fn get_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> WithMessages<Option<GotoDeclarationResponse>> {
        let mut msgs = self.remember(params.text_document_position_params.text_document.uri);
        let position = params.text_document_position_params.position;
        let standpoints = self.standpoints.lock().unwrap();
        let (standpoint, module) = match self.get_cached(&standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let time = std::time::Instant::now();
        let response = match_pos_to_symbol(standpoint, module.path_idx, position)
            .map(|(_, symbol)| {
                let declaration_opt =
                    symbol
                        .references
                        .first()
                        .map(|reference| SemanticSymbolDeclaration {
                            module_path: &standpoint
                                .module_map
                                .get(reference.module_path)
                                .expect("Could not retrieve path from index")
                                .path_buf,
                            span: &symbol.origin_span,
                        });
                let declaration = match declaration_opt {
                    Some(declaration) => declaration,
                    None => {
                        msgs.inform("Could not retrieve symbol's first instance.");
                        return None;
                    }
                };
                let uri = match path_to_uri(declaration.module_path) {
                    Ok(uri) => uri,
                    Err(_) => {
                        msgs.error("Could not convert path of declaration to uri!!!");
                        return None;
                    }
                };
                let range = to_range(*declaration.span);
                msgs.inform(format!("Retieved declaration in {:?}", time.elapsed()));
                Some(GotoDeclarationResponse::Scalar(Location::new(uri, range)))
            })
            .flatten();
        (msgs, response)
    }

    /// Get diagnostics.
    pub fn get_diagnostics(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Option<DocumentDiagnosticReport> {
        let uri = params.text_document.uri;
        let path_buf = uri_to_absolute_path(uri).ok()?;
        let contexts = self.standpoints.lock().unwrap();
        let standpoint = contexts
            .iter()
            .find(|context| context.contains_file(&path_buf))?;
        let path_idx = standpoint
            .module_map
            .paths()
            .find(|tuple| tuple.1.path_buf == path_buf)?
            .0;
        Some({
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: standpoint
                        .errors
                        .iter()
                        .filter(|error| error.offending_file == path_idx)
                        .map(|p| error_to_diagnostic(p, standpoint))
                        .collect::<Vec<Diagnostic>>(),
                },
            })
        })
    }

    /// Get workspace diagnostics.
    pub fn get_workspace_diagnostics(
        &self,
        _params: WorkspaceDiagnosticParams,
    ) -> WithMessages<WorkspaceDiagnosticReportResult> {
        let msgs = MessageStore::new();
        // let time = std::time::Instant::now();
        let report_result = self.get_or_compute_workspace_diagnostics();
        // msgs.inform(format!(
        //     "Retreived workspace diagnostics in {:?}",
        //     time.elapsed()
        // ));
        (msgs, report_result)
    }

    /// Gets the references for a symbol.
    pub fn get_references(&self, params: ReferenceParams) -> WithMessages<Option<Vec<Location>>> {
        let mut msgs = self.remember(params.text_document_position.text_document.uri);
        let position = params.text_document_position.position;
        let standpoints = self.standpoints.lock().unwrap();
        let (standpoint, module) = match self.get_cached(&standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let response =
            match_pos_to_symbol(standpoint, module.path_idx, position).map(|(_, symbol)| {
                let mut locations = vec![];
                for reference in &symbol.references {
                    let module_path_index = reference.module_path;
                    let module_path = match standpoint.module_map.get(module_path_index) {
                        Some(module) => &module.path_buf,
                        None => continue,
                    };
                    let uri = match path_to_uri(&module_path) {
                        Ok(uri) => uri,
                        Err(()) => continue,
                    };
                    for start in &reference.starts {
                        let span = Span::on_line(*start, symbol.name.len() as u32);
                        let location = Location {
                            uri: uri.clone(),
                            range: to_range(span),
                        };
                        locations.push(location);
                    }
                }
                locations
            });

        (msgs, response)
    }

    /// Rename a symbol.
    pub fn rename(
        &self,
        params: RenameParams,
    ) -> WithMessages<Result<Option<WorkspaceEdit>, Error>> {
        let mut msgs = MessageStore::new();
        let new_name = params.new_name;
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let pathbuf = match uri_to_absolute_path(uri) {
            Ok(path) => path,
            Err(_) => return (msgs, Err(Error::parse_error())),
        };
        if new_name.len() == 0 {
            msgs.inform("Could not rename: Empty name.");
            return (
                msgs,
                Err(Error::invalid_params(format!(
                    "Could not rename with empty name."
                ))),
            );
        }
        if is_keyword_or_operator(&new_name)
            || !is_valid_identifier_start(new_name.chars().next().unwrap())
            || new_name.chars().any(|ch| !is_valid_identifier(ch))
        {
            msgs.inform("Attempting to rename with keyword or operator.");
            return (
                msgs,
                Err(Error::invalid_params(format!(
                    "{new_name} is not a valid identifier."
                ))),
            );
        }
        let mut standpoints = self.standpoints.lock().unwrap();
        let standpoint = standpoints
            .iter_mut()
            .find(|context| context.contains_file(&pathbuf));
        let standpoint = match standpoint {
            Some(s) => s,
            None => return (
                msgs,
                Err(Error::invalid_params(
                    "Could not find module with this file, and therefore no changes are handled.",
                )),
            ),
        };
        let path_idx_opt = standpoint.module_map.map_path_to_index(&pathbuf);
        if path_idx_opt.is_none() {
            msgs.inform("Found standpoint, but could not map path to index");
            return (msgs, Ok(None));
        }
        let path_idx = path_idx_opt.unwrap();
        let response = match_pos_to_symbol(standpoint, path_idx, position).map(|(_, symbol)| {
            let mut edits = HashMap::new();
            for reference in &symbol.references {
                let mut textedits = vec![];
                let module_path_index = reference.module_path;
                let module_path = match standpoint.module_map.get(module_path_index) {
                    Some(module) => &module.path_buf,
                    None => continue,
                };
                let uri = match path_to_uri(&module_path) {
                    Ok(uri) => uri,
                    Err(()) => continue,
                };

                for start in &reference.starts {
                    let span = Span::on_line(*start, symbol.name.len() as u32);
                    let textedit = TextEdit {
                        range: to_range(span),
                        new_text: new_name.clone(),
                    };
                    textedits.push(textedit);
                }
                edits.insert(uri, textedits);
            }
            WorkspaceEdit {
                changes: Some(edits),
                document_changes: None, // todo: change a module file name.
                change_annotations: None,
            }
        });
        return (msgs, Ok(response));
    }

    /// Support for inlay hints.
    pub fn get_hints(&self, params: InlayHintParams) -> WithMessages<Option<Vec<InlayHint>>> {
        let mut msgs = self.remember(params.text_document.uri);
        let standpoints = self.standpoints.lock().unwrap();
        let (standpoint, module) = match self.get_cached(&standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let mut hints: Vec<InlayHint> = vec![];
        for symbol in standpoint.symbol_table.in_module(module.path_idx) {
            match &symbol.kind {
                SemanticSymbolKind::Variable {
                    declared_type,
                    inferred_type,
                    ..
                }
                | SemanticSymbolKind::Parameter {
                    param_type: declared_type,
                    inferred_type,
                    ..
                } => {
                    let entry_span = Span::on_line(
                        symbol.references.first().unwrap().starts[0],
                        symbol.name.len() as u32,
                    );
                    let position = to_range(entry_span).end;
                    let label_text = format!(
                        ": {}",
                        standpoint.symbol_table.format_evaluated_type(inferred_type)
                    );
                    if label_text.len() > 48 {
                        continue;
                    }
                    let label = InlayHintLabel::String(label_text);
                    if declared_type.is_none() {
                        hints.push(InlayHint {
                            position,
                            label,
                            kind: Some(InlayHintKind::TYPE),
                            padding_right: Some(true),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            data: None,
                        })
                    }
                }
                _ => (),
            }
        }
        (msgs, Some(hints))
    }

    /// Returns the last workspace diagnostics report, or computes another one if there was a file change.
    fn get_or_compute_workspace_diagnostics(&self) -> WorkspaceDiagnosticReportResult {
        let mut standpoints = self.standpoints.lock().unwrap();
        let mut was_updated = self.was_updated.lock().unwrap();
        let mut diagnostic_report = self.diagnostic_report.lock().unwrap();
        if *was_updated {
            standpoints.iter_mut().for_each(|standpoint| {
                standpoint.refresh_imports();
            });
            *diagnostic_report =
                WorkspaceDiagnosticReportResult::Report(WorkspaceDiagnosticReport {
                    items: match standpoints.first() {
                        Some(standpoint) => standpoint
                            .module_map
                            .paths()
                            .filter_map(|(path_idx, module)| {
                                Some(WorkspaceDocumentDiagnosticReport::Full(
                                    WorkspaceFullDocumentDiagnosticReport {
                                        uri: path_to_uri(&module.path_buf).ok()?,
                                        version: None,
                                        full_document_diagnostic_report:
                                            FullDocumentDiagnosticReport {
                                                result_id: None,
                                                items: standpoint
                                                    .errors
                                                    .iter()
                                                    .filter(|error| {
                                                        error.offending_file == path_idx
                                                    })
                                                    .map(|p| error_to_diagnostic(p, standpoint))
                                                    .collect::<Vec<Diagnostic>>(),
                                            },
                                    },
                                ))
                            })
                            .collect(),

                        None => vec![],
                    },
                });
            *was_updated = false;
        }
        diagnostic_report.clone()
    }
}

/// Hacky hack.
/// Calculates the maximum number of milliseconds a text change can take
/// before it is necessary to trigger a full refresh.
/// The values are chosen arbitrarily.
/// If a standpoint takes 10 seconds to handle a text change, it will restart.
fn get_time_limit(module_map: &analyzer::ModuleMap) -> u64 {
    match module_map.len() {
        0..=99 => {
            // If there is a file with at least one thousand lines.
            if module_map
                .paths()
                .any(|(_, idx)| idx.line_lengths.len() > 2000)
            {
                1200
            } else {
                600
            }
        }
        100..=999 => 2000,
        1000..=1500 => 4000,
        1600..=2000 => 8000,
        _ => 10000,
    }
}

fn is_valid_complete_target(
    sym: &SemanticSymbol,
    module: &TypedModule,
    standpoint: &Standpoint,
    pos: [u32; 2],
    closest_symbol: Option<(SymbolIndex, &SemanticSymbol)>,
    trigger: Trigger,
) -> bool {
    let is_module_local = sym.was_declared_in(module.path_idx)
        && !matches!(&sym.kind, SemanticSymbolKind::Module { .. });
    let is_prelude_symbol = standpoint
        .prelude_path
        .is_some_and(|path| sym.was_declared_in(path))
        && sym.kind.is_public();
    let valid_location = match &sym.kind {
        SemanticSymbolKind::Variable { .. } | SemanticSymbolKind::Constant { .. } => {
            sym.ident_span().is_before(Span::at(pos))
        }
        _ => true,
    };
    let closest = closest_symbol.is_some() && {
        let (_, symbol) = closest_symbol.as_ref().unwrap();
        sym.name.chars().next().unwrap().to_lowercase().eq(symbol
            .name
            .chars()
            .next()
            .unwrap()
            .to_lowercase())
            && sym
                .name
                .to_lowercase()
                .starts_with(&symbol.name.to_lowercase())
    } || closest_symbol.is_none();
    let is_valid_symbol = ((is_module_local && valid_location) || is_prelude_symbol) && closest;
    (match trigger {
        Trigger::NewInstance => match &sym.kind {
            SemanticSymbolKind::Model {
                is_constructable, ..
            } => *is_constructable,
            _ => false,
        },
        Trigger::Implements => match &sym.kind {
            SemanticSymbolKind::Trait { .. } => true,
            _ => false,
        },
        Trigger::TypeLabel => matches!(
            &sym.kind,
            SemanticSymbolKind::TypeName { .. }
                | SemanticSymbolKind::Enum { .. }
                | SemanticSymbolKind::Model { .. }
        ),
        _ => true,
    }) && is_valid_symbol
}

/// Returns the symbol that is closest to a completion trigger.
fn get_closest_symbol<'a>(
    symboltable: &'a analyzer::SymbolTable,
    module: &'a TypedModule,
    pos: [u32; 2],
    trigger_is_dot: bool,
) -> Option<(SymbolIndex, &'a SemanticSymbol)> {
    for (index, symbol) in symboltable.symbols() {
        for reference in &symbol.references {
            if reference.module_path == module.path_idx {
                for start in &reference.starts {
                    let span = Span::on_line(*start, symbol.name.len() as u32);
                    if span.is_before(Span::at(pos)) && span.is_adjacent_to(pos)
                        || (!trigger_is_dot && span.contains(pos))
                    {
                        let index = symboltable.forward(index);
                        let symbol = symboltable.get_forwarded(index)?;
                        return Some((index, symbol));
                    }
                }
            }
        }
    }
    None
}

/// Convert a uri to an absolute path.
pub fn uri_to_absolute_path(uri: Url) -> Result<PathBuf, DocumentError> {
    Ok(uri
        .to_file_path()
        .or_else(|_| Err(DocumentError::CouldNotConvert(uri)))?
        .canonicalize()?)
}

/// Convert a path to a uri.
pub fn path_to_uri(path: &Path) -> Result<Url, ()> {
    Url::from_file_path(path)
}

// Match a text document position to a symbol in the symbol table.
// todo: find a way to find module symbols without iterating through all symbols.
pub fn match_pos_to_symbol<'a>(
    standpoint: &'a Standpoint,
    path_idx: PathIndex,
    position: Position,
) -> Option<(SymbolIndex, &'a SemanticSymbol)> {
    // Editor ranges are zero-based, for some reason.
    let pos = [position.line + 1, position.character + 1];
    for (symbol_idx, symbol) in standpoint.symbol_table.symbols() {
        for reference in &symbol.references {
            if reference.module_path == path_idx {
                for start in &reference.starts {
                    let span = Span::on_line(*start, symbol.name.len() as u32);
                    if span.contains(pos) {
                        return if let SemanticSymbolKind::Import {
                            source: Some(source),
                            ..
                        } = &symbol.kind
                        {
                            // Import redirection.
                            let mut origin = source;
                            let mut parent = standpoint.symbol_table.get(*source);
                            while let Some(SemanticSymbol {
                                kind:
                                    SemanticSymbolKind::Import {
                                        source: Some(source),
                                        ..
                                    },
                                ..
                            }) = parent
                            {
                                origin = source;
                                parent = standpoint.symbol_table.get(*source);
                            }
                            parent.map(|symbol| (*origin, symbol))
                        } else if let SemanticSymbolKind::Property {
                            resolved: Some(source),
                            is_opaque,
                        } = &symbol.kind
                        {
                            if *is_opaque {
                                None
                            } else {
                                // Property redirection.
                                standpoint
                                    .symbol_table
                                    .get(*source)
                                    .map(|symbol| (*source, symbol))
                            }
                        } else {
                            Some((symbol_idx, symbol))
                        };
                    }
                }
            }
        }
    }
    return None;
}
