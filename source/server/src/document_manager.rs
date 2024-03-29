use crate::{
    completion::{sort_completions, CompletionFinder, DotCompletionType, Trigger},
    diagnostic::{progdiagnostic_to_diagnostic, to_range},
    error::DocumentError,
    // folding_range::FoldingRangeFinder,
    hover::{HoverFinder, HoverInfo},
    message_store::{MessageStore, WithMessages},
};
use analyzer::{
    utils::symbol_to_type, Module, PathIndex, SemanticSymbol, SemanticSymbolDeclaration,
    SemanticSymbolKind, Standpoint, SymbolIndex, TypedModule, TypedVisitorNoArgs,
};
use ast::{
    is_keyword_or_operator, is_valid_identifier, is_valid_identifier_start, unwrap_or_continue,
    Span,
};
use pretty::SymbolWriter;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    str::FromStr,
    sync::{Arc, Mutex},
};
use tower_lsp::{
    jsonrpc::Error,
    lsp_types::{
        request::{GotoDeclarationParams, GotoDeclarationResponse},
        CompletionContext,
        CompletionItem,
        CompletionItemKind,
        CompletionParams,
        CompletionResponse,
        Diagnostic,
        DidChangeTextDocumentParams,
        DidOpenTextDocumentParams,
        DidSaveTextDocumentParams,
        DocumentDiagnosticParams,
        DocumentDiagnosticReport,
        DocumentSymbol,
        DocumentSymbolParams,
        DocumentSymbolResponse, //FoldingRange,
        /* FoldingRangeParams */ FullDocumentDiagnosticReport,
        HoverParams,
        InlayHint,
        InlayHintKind,
        InlayHintLabel,
        InlayHintParams,
        Location,
        Position,
        ReferenceParams,
        RelatedFullDocumentDiagnosticReport,
        RenameParams,
        SymbolKind,
        TextEdit,
        Url,
        WorkspaceDiagnosticParams,
        WorkspaceDiagnosticReport,
        WorkspaceDiagnosticReportResult,
        WorkspaceDocumentDiagnosticReport,
        WorkspaceEdit,
        WorkspaceFullDocumentDiagnosticReport,
    },
};
use utils::get_parent_dir;

/// Adds an error message to a message store and immediately returns the message store.
macro_rules! error {
    ($messages: expr, $($arg:tt)*) => {{
        $messages.error((format!($($arg)*)));
        return $messages;
    }};
}

#[derive(Debug)]
pub struct DocumentManager {
    pub standpoint: Arc<Mutex<Standpoint>>,
    /// Boolean stopper to prevent unnecessary workspace diagnostic refreshes.
    was_updated: Arc<Mutex<bool>>,
    /// Cached workspace diagnostic report.
    _diagnostic_report: Arc<Mutex<WorkspaceDiagnosticReportResult>>,
    /// Last opened standpoint module.
    last_opened: Arc<Mutex<Option<PathIndex>>>,
}

impl DocumentManager {
    /// Creates a new document manager.
    pub fn new(corelib_path: Option<PathBuf>) -> Self {
        let mut standpoint = Standpoint::new(true, corelib_path);
        standpoint.validate();
        DocumentManager {
            standpoint: Arc::new(Mutex::new(standpoint)),
            was_updated: Arc::new(Mutex::new(true)),
            _diagnostic_report: Arc::new(Mutex::new(WorkspaceDiagnosticReportResult::Report(
                Default::default(),
            ))),
            last_opened: Arc::new(Mutex::new(None)),
        }
    }

    /// Handle the opening of a document/module.
    pub fn open_document(&self, params: DidOpenTextDocumentParams) {
        let path_buf = match uri_to_absolute_path(params.text_document.uri.clone()) {
            Ok(path_buf) => path_buf,
            Err(_) => return,
        };
        let mut standpoint = self.standpoint.lock().unwrap();
        if let Some(module) = standpoint.module_map.map_path_to_index(&path_buf) {
            standpoint.refresh_module(module, &params.text_document.text);
        }
    }

    /// Sets the open module.
    pub fn remember(&self, uri: Url) -> MessageStore {
        let mut msgs = MessageStore::new();
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path_buf) => path_buf,
            Err(err) => {
                error!(
                    msgs,
                    "Could not convert URI to an absolute file path. ERROR: {err:?}"
                )
            }
        };
        let standpoint = self.standpoint.lock().unwrap();
        match self.get_cached(&standpoint) {
            Some(module) => {
                if module.path_buf == path_buf {
                    // Document/module is already opened.
                    return msgs;
                }
                std::mem::drop(standpoint);
                self.update_cached(path_buf, &mut msgs);
            }
            None => {
                std::mem::drop(standpoint);
                self.update_cached(path_buf, &mut msgs);
            }
        };
        msgs
    }

    /// Handles a file save in the editor.
    pub fn save_file(&self, _: DidSaveTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();
        let time = std::time::Instant::now();
        msgs.inform("Saving file...");
        let mut standpoint = self.standpoint.lock().unwrap();
        standpoint.validate();
        *self.was_updated.lock().unwrap() = true;
        msgs.inform(format!(
            "File saved. Workspace updated in {:?}",
            time.elapsed()
        ));
        msgs
    }

    /// Update the cached module.
    fn update_cached(&self, path_buf: PathBuf, msgs: &mut MessageStore) {
        let standpoint = self.standpoint.lock().unwrap();
        let module_idx = match standpoint.module_map.map_path_to_index(&path_buf) {
            Some(path_idx) => path_idx,
            None => {
                msgs.error("Could not retrieve cached module.");
                return;
            }
        };
        let mut last_opened = self.last_opened.lock().unwrap();
        *last_opened = Some(module_idx);
    }

    /// Return the cached module.
    fn get_cached<'a>(&self, standpoint: &'a Standpoint) -> Option<&'a TypedModule> {
        let last_opened = self.last_opened.lock().unwrap();
        let last_opened = last_opened.as_ref()?;
        let last_opened_module = standpoint.module_map.get(*last_opened)?;
        Some(last_opened_module)
    }

    /// Add a new document to be tracked.
    pub fn add_document(&self, params: DidOpenTextDocumentParams) -> MessageStore {
        let mut msgs = MessageStore::new();
        let uri = params.text_document.uri;
        let path_buf = match uri_to_absolute_path(uri.clone()) {
            Ok(path_buf) => path_buf,
            Err(err) => {
                error!(
                    msgs,
                    "Could not convert URI to an absolute file path. ERROR: {err:?}"
                )
            }
        };
        let mut standpoint = self.standpoint.lock().unwrap();
        if standpoint.contains_file(&path_buf) {
            return msgs;
        }
        match Module::from_path(path_buf) {
            Ok(module) => {
                let path_idx = match standpoint.add_module(module) {
                    Some(path_idx) => {
                        msgs.inform(format!(
                            "Module added at index {path_idx:?}. {} modules in standpoint.",
                            standpoint.module_map.len()
                        ));
                        path_idx
                    }
                    None => {
                        msgs.error("The module was not added. Something went wrong.");
                        return msgs;
                    }
                };
                standpoint.validate();
                standpoint.refresh_module(path_idx, &params.text_document.text);
            }
            // If module cannot be added, store error in the root file.
            Err(error) => {
                standpoint.add_import_error(error);
            }
        }
        return msgs;
    }

    /// Hover over support.
    pub fn get_hover_info(&self, params: HoverParams) -> WithMessages<Option<HoverInfo>> {
        let mut messages = self.remember(params.text_document_position_params.text_document.uri);
        let time = std::time::Instant::now();
        let standpoint = self.standpoint.lock().unwrap();
        let module = match self.get_cached(&standpoint) {
            Some(t) => t,
            None => {
                messages.error("Could not retrieve the cached module index");
                return (messages, None);
            }
        };
        let position = params.text_document_position_params.position;
        // Editor ranges are zero-based, for some reason.
        let pos = [position.line + 1, position.character + 1];
        let hover_finder = HoverFinder::new(module, &standpoint, pos, messages);
        let mut messages = MessageStore::new();
        for statement in module.statements.iter() {
            if let Some(hover) = hover_finder.statement(statement) {
                messages = hover_finder.message_store.take();
                messages.inform(format!("Retreived hover info in {:?}", time.elapsed()));
                return (messages, Some(hover));
            }
        }
        // Hover info not found. Is hover over an imported module?
        let symbollib = &standpoint.symbol_library;
        for (_, othermodule) in standpoint.module_map.paths() {
            let module_symbol = unwrap_or_continue!(symbollib.get(othermodule.symbol_idx));
            if !module_symbol.is_referenced_in(module.path_idx) {
                continue;
            }
            for reference in module_symbol.references.iter() {
                if reference.module_path != module.path_idx {
                    continue;
                }
                for start in reference.starts.iter() {
                    let span = Span::on_line(*start, module_symbol.name.len() as u32);
                    let hoverinfo = Some((&*standpoint, othermodule.symbol_idx).into());
                    if span.contains(pos) {
                        return (messages, hoverinfo);
                    }
                }
            }
        }
        return (hover_finder.message_store.take(), None);
    }

    /// Checks if a uri is already being tracked.
    pub fn has(&self, uri: Url) -> bool {
        let context = self.standpoint.lock().unwrap();
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path_buf) => path_buf,
            Err(_) => return false,
        };
        if context.contains_file(&path_buf) {
            return true;
        }
        return false;
    }

    /// Gets completion response.
    /// TODO: This will obviously get sloooooww. Fix.
    pub fn completion(&self, params: CompletionParams) -> WithMessages<Option<CompletionResponse>> {
        let mut msgs = self.remember(params.text_document_position.text_document.uri);
        let standpoint = self.standpoint.lock().unwrap();
        let module = match self.get_cached(&standpoint) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let position = params.text_document_position.position;
        // Editor ranges are zero-based, for some reason.
        let pos = [position.line + 1, position.character + 1];
        let time = std::time::Instant::now();
        let completion_finder =
            CompletionFinder::new(module, &standpoint, pos, msgs, params.context);
        for (index, statement) in module.statements.iter().enumerate() {
            completion_finder.next_statement_is(index + 1, &module.statements);
            if let Some(completions) = completion_finder.statement(statement) {
                let mut messages = completion_finder.message_store.take();
                messages.inform(format!("Retreived completions in {:?}", time.elapsed()));
                return (messages, Some(completions));
            }
        }
        let symbollib = &standpoint.symbol_library;
        let trigger = match &completion_finder.context {
            Some(CompletionContext {
                trigger_character, ..
            }) => match trigger_character {
                Some(trigger) => match trigger.as_str() {
                    "use " => Trigger::UseImport,
                    "new " => Trigger::NewInstance,
                    "implements " => Trigger::Implements,
                    "module " => Trigger::Module,
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
                let closest = get_closest_symbol(symbollib, module, pos, trigger_is_dot);
                if closest.is_some() {
                    let (index, symbol) = closest.unwrap();
                    let inferred_type = match symbol_to_type(symbol, index, symbollib) {
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
                let writer = SymbolWriter::new(&standpoint);
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
                        let symbol = standpoint.symbol_library.get(symbol_idx)?;
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
            Trigger::Module => {
                let filename = module.path_buf.file_stem().and_then(|stem| stem.to_str());
                if let Some(filename) = filename {
                    let completion_item = CompletionItem {
                        label: filename.to_string(),
                        kind: Some(CompletionItemKind::MODULE),
                        ..Default::default()
                    };
                    let response = CompletionResponse::Array(vec![completion_item]);
                    return (completion_finder.message_store.take(), Some(response));
                }
            }
            trigger => {
                // Attempt regular autocomplete.
                // Preventing completion in comments should be handled by the client.
                let closest = get_closest_symbol(symbollib, module, pos, trigger_is_dot);
                let mut completions = vec![];
                let writer = SymbolWriter::new(&standpoint);
                for (index, symbol) in symbollib.symbols().filter(|(_, sym)| {
                    is_valid_complete_target(sym, module, &standpoint, pos, closest, trigger)
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
        let mut standpoint = self.standpoint.lock().unwrap();
        let path_idx = match self.last_opened.lock().unwrap().clone() {
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
            Some(()) => {
                msgs.inform(format!("Document refreshed in {:?}", time.elapsed(),));
            }
            _ => error!(msgs, "Something went wrong while refreshing user text."),
        };
        *self.was_updated.lock().unwrap() = true;
        msgs
    }

    /// Returns the first declaration of a symbol.
    pub fn get_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> WithMessages<Option<GotoDeclarationResponse>> {
        let mut msgs = self.remember(params.text_document_position_params.text_document.uri);
        let position = params.text_document_position_params.position;
        let standpoint = self.standpoint.lock().unwrap();
        let standpoint = &standpoint;
        let module = match self.get_cached(standpoint) {
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
        let standpoint = self.standpoint.lock().unwrap();
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
                        .diagnostics
                        .iter()
                        .filter(|error| error.offending_file == path_idx)
                        .map(|p| progdiagnostic_to_diagnostic(p))
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
        let standpoint = self.standpoint.lock().unwrap();
        let standpoint = &standpoint;
        let module = match self.get_cached(&standpoint) {
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
        let standpoint = self.standpoint.lock().unwrap();
        let standpoint = &standpoint;
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
        let standpoint = self.standpoint.lock().unwrap();
        let module = match self.get_cached(&standpoint) {
            Some(t) => t,
            None => {
                msgs.error("Could not retrieve the cached module index");
                return (msgs, None);
            }
        };
        let mut hints: Vec<InlayHint> = vec![];
        for symbol in standpoint.symbol_library.in_module(module.path_idx) {
            let inferred_type = match &symbol.kind {
                SemanticSymbolKind::Variable {
                    declared_type,
                    inferred_type,
                    ..
                }
                | SemanticSymbolKind::Parameter {
                    param_type: declared_type,
                    inferred_type,
                    ..
                } if declared_type.is_none() => inferred_type,
                SemanticSymbolKind::LoopVariable { inferred_type, .. } => inferred_type,
                _ => continue,
            };
            let entry_span = Span::on_line(
                symbol.references.first().unwrap().starts[0],
                symbol.name.len() as u32,
            );
            let position = to_range(entry_span).end;
            let label_text = format!(
                ": {}",
                standpoint
                    .symbol_library
                    .format_evaluated_type(inferred_type)
            );
            if label_text.len() > 48 {
                continue;
            }
            let label = InlayHintLabel::String(label_text);

            hints.push(InlayHint {
                position,
                label,
                kind: Some(InlayHintKind::TYPE),
                padding_right: Some(true),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                data: None,
            });
        }
        (msgs, Some(hints))
    }

    /// Returns the last workspace diagnostics report, or computes another one if there was a file change.
    fn get_or_compute_workspace_diagnostics(&self) -> WorkspaceDiagnosticReportResult {
        let standpoint = self.standpoint.lock().unwrap();
        // let mut was_updated = self.was_updated.lock().unwrap();
        // let mut diagnostic_report = self.diagnostic_report.lock().unwrap();
        // if *was_updated {
        // *diagnostic_report =
        WorkspaceDiagnosticReportResult::Report(WorkspaceDiagnosticReport {
            items: standpoint
                .module_map
                .paths()
                .filter_map(|(path_idx, module)| {
                    Some(WorkspaceDocumentDiagnosticReport::Full(
                        WorkspaceFullDocumentDiagnosticReport {
                            uri: path_to_uri(&module.path_buf).ok()?,
                            version: None,
                            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                                result_id: None,
                                items: standpoint
                                    .diagnostics
                                    .iter()
                                    .filter(|error| error.offending_file == path_idx)
                                    .map(|p| progdiagnostic_to_diagnostic(p))
                                    .collect::<Vec<Diagnostic>>(),
                            },
                        },
                    ))
                })
                .collect(),
        })
        // *was_updated = false;
        // }
        // diagnostic_report.clone()
    }

    pub fn get_symbols(&self, params: DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
        self.remember(params.text_document.uri);
        let standpoint = self.standpoint.lock().unwrap();
        let module = self.get_cached(&standpoint)?;
        let symbol_library = &standpoint.symbol_library;

        let symbols = symbol_library
            .symbols()
            .filter(|(_, symbol)| {
                symbol.was_declared_in(module.path_idx)
                    && !matches!(
                        &symbol.kind,
                        SemanticSymbolKind::Module { .. }
                            | SemanticSymbolKind::Import { .. }
                            | SemanticSymbolKind::Property { .. }
                    )
            })
            .map(|(idx, _)| {
                let symbol = symbol_library.get(idx).unwrap();
                let doc_symbol = doc_symbol_from_semantic_symbol(symbol, symbol_library);
                doc_symbol
            })
            .collect::<Vec<_>>();
        return Some(DocumentSymbolResponse::Nested(symbols));
    }
}

fn generate_doc_symbol<'a, T: 'a + Iterator<Item = &'a SymbolIndex>>(
    iter: T,
    symbol_library: &analyzer::SymbolLibrary,
) -> Vec<DocumentSymbol> {
    iter.filter_map(|idx| {
        symbol_library
            .get(*idx)
            .map(|symbol| doc_symbol_from_semantic_symbol(symbol, symbol_library))
    })
    .collect()
}

fn doc_symbol_from_semantic_symbol(
    symbol: &SemanticSymbol,
    symbol_library: &analyzer::SymbolLibrary,
) -> DocumentSymbol {
    let (kind, detail) = get_symbol_kind_and_detail(symbol, symbol_library);
    #[allow(deprecated)]
    let doc_symbol = DocumentSymbol {
        name: symbol.name.clone(),
        detail,
        kind,
        range: to_range(symbol.origin_span),
        selection_range: to_range(symbol.ident_span()),
        children: match &symbol.kind {
            SemanticSymbolKind::Interface { methods, .. } => {
                Some(generate_doc_symbol(methods.iter(), symbol_library))
            }
            SemanticSymbolKind::Model {
                methods,
                attributes,
                ..
            } => Some(generate_doc_symbol(
                methods.iter().chain(attributes.iter()),
                symbol_library,
            )),
            SemanticSymbolKind::Enum { variants, .. } => {
                Some(generate_doc_symbol(variants.iter(), symbol_library))
            }
            // SemanticSymbolKind::Variant {
            //     owner_enum,
            //     variant_index,
            //     tagged_types,
            // } => todo!(),
            // SemanticSymbolKind::Variable {
            //     pattern_type,
            //     is_public,
            //     declared_type,
            //     inferred_type,
            // } => todo!(),
            // SemanticSymbolKind::Constant {
            //     is_public,
            //     declared_type,
            //     inferred_type,
            // } => todo!(),
            // SemanticSymbolKind::Attribute {
            //     owner_model,
            //     is_public,
            //     property_index,
            //     declared_type,
            // } => todo!(),
            // SemanticSymbolKind::Method {
            //     is_public,
            //     is_static,
            //     is_async,
            //     owner_model_or_interface,
            //     property_index,
            //     params,
            //     generic_params,
            //     return_type,
            // } => todo!(),
            // SemanticSymbolKind::Parameter {
            //     is_optional,
            //     param_type,
            //     inferred_type,
            // } => todo!(),
            // SemanticSymbolKind::GenericParameter {
            //     interfaces,
            //     default_value,
            // } => todo!(),
            // SemanticSymbolKind::Function {
            //     is_public,
            //     is_async,
            //     params,
            //     generic_params,
            //     return_type,
            // } => todo!(),
            _ => None,
        },
        deprecated: None,
        tags: None,
    };
    doc_symbol
}

fn get_symbol_kind_and_detail(
    symbol: &SemanticSymbol,
    symbol_library: &analyzer::SymbolLibrary,
) -> (SymbolKind, Option<String>) {
    let (kind, detail) = match &symbol.kind {
        SemanticSymbolKind::Interface { .. } => (SymbolKind::INTERFACE, None),
        SemanticSymbolKind::Model { .. } => (SymbolKind::CLASS, None),
        SemanticSymbolKind::Enum { .. } => (SymbolKind::ENUM, None),
        SemanticSymbolKind::Variable { inferred_type, .. } => (
            SymbolKind::VARIABLE,
            Some(symbol_library.format_evaluated_type(inferred_type)),
        ),
        SemanticSymbolKind::Function { .. } => (SymbolKind::FUNCTION, None),
        SemanticSymbolKind::TypeName { .. } => (SymbolKind::INTERFACE, None),
        SemanticSymbolKind::Attribute { .. } => (SymbolKind::FIELD, None),
        SemanticSymbolKind::Parameter { .. } => (SymbolKind::VARIABLE, None),
        SemanticSymbolKind::GenericParameter { .. } => (SymbolKind::TYPE_PARAMETER, None),
        SemanticSymbolKind::Method { .. } => (SymbolKind::METHOD, None),
        SemanticSymbolKind::Variant { .. } => (SymbolKind::CONSTANT, None),
        _ => (SymbolKind::KEY, None),
    };
    (kind, detail)
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
        SemanticSymbolKind::Variable { .. } => sym.ident_span().is_before(Span::at(pos)),
        _ => true,
    };
    let closest = (closest_symbol.is_some() && {
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
    }) || closest_symbol.is_none();
    let is_valid_symbol = ((is_module_local && valid_location) || is_prelude_symbol) && closest;
    (match trigger {
        Trigger::NewInstance => match &sym.kind {
            SemanticSymbolKind::Model {
                is_constructable, ..
            } => *is_constructable,
            _ => false,
        },
        Trigger::Implements => match &sym.kind {
            SemanticSymbolKind::Interface { .. } => true,
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
    symbollib: &'a analyzer::SymbolLibrary,
    module: &'a TypedModule,
    pos: [u32; 2],
    trigger_is_dot: bool,
) -> Option<(SymbolIndex, &'a SemanticSymbol)> {
    for (index, symbol) in symbollib.symbols() {
        for reference in &symbol.references {
            if reference.module_path == module.path_idx {
                for start in &reference.starts {
                    let span = Span::on_line(*start, symbol.name.len() as u32);
                    if span.is_before(Span::at(pos)) && span.is_adjacent_to(pos)
                        || (!trigger_is_dot && span.contains(pos))
                    {
                        let index = symbollib.forward(index);
                        let symbol = symbollib.get_forwarded(index)?;
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
    match uri
        .to_file_path()
        .map(|path| PathBuf::from_str(path.to_string_lossy().to_string().as_str()))
    {
        Ok(path) => path.map_err(|_| DocumentError::CouldNotConvert(uri)),
        Err(_) => Err(DocumentError::CouldNotConvert(uri)),
    }
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
    for (symbol_idx, symbol) in standpoint.symbol_library.symbols() {
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
                            let mut parent = standpoint.symbol_library.get(*source);
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
                                parent = standpoint.symbol_library.get(*source);
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
                                    .symbol_library
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
