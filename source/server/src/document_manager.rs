use crate::{
    completion::{CompletionFinder, DotCompletionType},
    diagnostic::{error_to_diagnostic, to_range},
    error::DocumentError,
    hover::{HoverFinder, HoverInfo},
    message_store::{MessageStore, WithMessages},
};
use analyzer::{
    utils::symbol_to_type, Module, PathIndex, SemanticSymbol, SemanticSymbolDeclaration,
    SemanticSymbolKind, Standpoint, StandpointStatus, SymbolIndex, TypedModule, TypedVisitorNoArgs,
    CORE_LIBRARY_PATH,
};
use ast::{is_keyword_or_operator, is_valid_identifier, is_valid_identifier_start, Span};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::RwLock,
};
use tower_lsp::{
    jsonrpc::Error,
    lsp_types::{
        request::{GotoDeclarationParams, GotoDeclarationResponse},
        CompletionParams, CompletionResponse, Diagnostic, DidChangeTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams,
        DocumentDiagnosticReport, FullDocumentDiagnosticReport, HoverParams, InlayHint,
        InlayHintKind, InlayHintLabel, InlayHintParams, Location, Position, ReferenceParams,
        RelatedFullDocumentDiagnosticReport, RenameParams, TextEdit, Url,
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
    pub standpoints: RwLock<Vec<Standpoint>>,
    pub corelib_path: Option<PathBuf>,
    /// Boolean stopper to prevent unnecessary workspace diagnostic refreshes.
    was_updated: RwLock<bool>,
    /// Cached workspace diagnostic report.
    diagnostic_report: RwLock<WorkspaceDiagnosticReportResult>,
    /// Last opened standpoint module.
    last_opened: RwLock<Option<(usize, PathIndex)>>,
}

impl DocumentManager {
    /// Creates a new document manager.
    pub fn new() -> Self {
        DocumentManager {
            standpoints: RwLock::new(vec![]),
            corelib_path: Some(PathBuf::from(CORE_LIBRARY_PATH)),
            was_updated: RwLock::new(true),
            diagnostic_report: RwLock::new(WorkspaceDiagnosticReportResult::Report(
                Default::default(),
            )),
            last_opened: RwLock::new(None),
        }
    }

    /// Sets the open module.
    pub fn open_document(&self, uri: Url) -> MessageStore {
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
        let standpoints = self.standpoints.read().unwrap();
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
        for standpoint in self.standpoints.write().unwrap().iter_mut() {
            standpoint.refresh_imports();
            standpoint.check_all_modules();
        }
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
        let mut last_opened = self.last_opened.write().unwrap();
        *last_opened = Some((index, module_idx));
    }

    /// Return the cached module.
    fn get_cached<'a>(
        &self,
        standpoints: &'a Vec<Standpoint>,
    ) -> Option<(&'a Standpoint, &'a TypedModule)> {
        let last_opened = self.last_opened.read().unwrap();
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
        let last_opened = self.last_opened.read().unwrap();
        let last_opened = last_opened.as_ref()?;
        let last_opened_standpoint = standpoints.get_mut(last_opened.0)?;
        let last_opened_module = last_opened_standpoint.module_map.get_mut(last_opened.1)?;
        let idx = last_opened_module.path_idx.clone();
        Some((last_opened_standpoint, idx))
    }

    /// Add a new document to be tracked.
    pub fn add_document(&self, params: DidOpenTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();
        let mut standpoints = self.standpoints.write().unwrap();
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
                let index_of_current_module = if !standpoint.contains_file(&path_buf) {
                    match Module::from_path(path_buf) {
                        Ok(current_module) => match standpoint.add_module(current_module) {
                            Some(index) => index,
                            None => {
                                log_error!(
                                msgs,
                                "Could not add this module to fresh standpoint. Skipping altogether.."
                            )
                            }
                        },
                        Err(error) => {
                            msgs.inform(format!("Error creating current module: {error:?}"));
                            standpoints.push(standpoint);
                            return msgs;
                        }
                    }
                } else {
                    standpoint.module_map.map_path_to_index(&path_buf).unwrap()
                };

                // todo: this is a hack to overshadow ghost initial errors. fix.
                msgs.inform("Module added successfully.");
                standpoint.refresh_module(index_of_current_module, params.text_document.text, true);
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
        let mut messages =
            self.open_document(params.text_document_position_params.text_document.uri);
        let time = std::time::Instant::now();
        let standpoints = self.standpoints.read().unwrap();
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
        let contexts = self.standpoints.read().unwrap();
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
    pub fn completion(&self, params: CompletionParams) -> WithMessages<Option<CompletionResponse>> {
        let mut msgs = self.open_document(params.text_document_position.text_document.uri);
        let standpoints = self.standpoints.read().unwrap();
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
        for (index, symbol) in symboltable.symbols() {
            for reference in &symbol.references {
                if reference.module_path == module.path_idx {
                    for start in &reference.starts {
                        let span = Span::on_line(*start, symbol.name.len() as u32);
                        if span.is_before(Span::at(pos)) && span.is_adjacent_to(pos) {
                            let index = symboltable.forward(index);
                            let symbol = symboltable.get_forwarded(index).unwrap();
                            let inferred_type = match symbol_to_type(symbol, index, symboltable) {
                                Ok(typ) => typ,
                                Err(_) => return (completion_finder.message_store.take(), None),
                            };
                            if completion_finder.trigger_character_is(".") {
                                let response = completion_finder
                                    .complete_from_dot(inferred_type, DotCompletionType::Half);
                                return (completion_finder.message_store.take(), response);
                            }
                        }
                    }
                }
            }
        }
        return (completion_finder.message_store.take(), None);
    }

    /// Handles a change in the text of a module.
    pub fn handle_change(&self, mut params: DidChangeTextDocumentParams) -> MessageStore {
        let uri = params.text_document.uri.clone();
        let mut msgs = self.open_document(uri.clone());
        msgs.inform("Handling text update...");
        let mut standpoints = self.standpoints.write().unwrap();
        let (standpoint, path_idx) = match self.get_cached_mut(&mut standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return msgs;
            }
        };
        let last = params.content_changes.len() - 1;
        let most_current = std::mem::take(&mut params.content_changes[last].text);
        std::mem::drop(params);
        msgs.inform(format!("Refreshing open document..."));
        let time = std::time::Instant::now();
        match standpoint.refresh_module(path_idx, most_current, false) {
            Some(StandpointStatus::RefreshSuccessful) => {
                msgs.inform(format!("Document refreshed in {:?}", time.elapsed()))
            }
            _ => log_error!(msgs, "Something went wrong while refreshing user text."),
        };
        *self.was_updated.write().unwrap() = true;
        // idk what to do here.
        if time.elapsed() > std::time::Duration::from_millis(300) {
            standpoint.restart();
        };
        msgs
    }

    /// Returns the first declaration of a symbol.
    pub fn get_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> WithMessages<Option<GotoDeclarationResponse>> {
        let mut msgs = self.open_document(params.text_document_position_params.text_document.uri);
        msgs.inform("Retreiving symbol declaration...");
        let position = params.text_document_position_params.position;
        let standpoints = self.standpoints.read().unwrap();
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
        let contexts = self.standpoints.read().unwrap();
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
        let mut msgs = self.open_document(params.text_document_position.text_document.uri);
        let position = params.text_document_position.position;
        let standpoints = self.standpoints.read().unwrap();
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
        let mut standpoints = self.standpoints.write().unwrap();
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
        let mut msgs = self.open_document(params.text_document.uri);
        let standpoints = self.standpoints.read().unwrap();
        let (standpoint, module) = match self.get_cached(&standpoints) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let mut hints: Vec<InlayHint> = vec![];
        for symbol in standpoint.symbol_table.in_module(module.path_idx) {
            if let SemanticSymbolKind::Variable {
                declared_type,
                inferred_type,
                ..
            } = &symbol.kind
            {
                let entry_span = Span::on_line(
                    symbol.references.first().unwrap().starts[0],
                    symbol.name.len() as u32,
                );
                let position = to_range(entry_span).end;
                let label_text = format!(
                    ": {}",
                    standpoint.symbol_table.format_evaluated_type(inferred_type)
                );
                if label_text.len() > 40 {
                    continue;
                }
                let label = InlayHintLabel::String(label_text);
                if declared_type.is_none() {
                    hints.push(InlayHint {
                        position,
                        label,
                        kind: Some(InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: None,
                        padding_right: Some(true),
                        data: None,
                    })
                }
            }
        }
        (msgs, Some(hints))
    }

    /// Returns the last workspace diagnostics report, or computes another one if there was a file change.
    fn get_or_compute_workspace_diagnostics(&self) -> WorkspaceDiagnosticReportResult {
        let mut standpoints = self.standpoints.write().unwrap();
        let mut was_updated = self.was_updated.write().unwrap();
        let mut diagnostic_report = self.diagnostic_report.write().unwrap();
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
                        } = &symbol.kind
                        {
                            // Property redirection.
                            standpoint
                                .symbol_table
                                .get(*source)
                                .map(|symbol| (*source, symbol))
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
