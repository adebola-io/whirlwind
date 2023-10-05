use std::{path::PathBuf, sync::RwLock};

use crate::{
    diagnostic::to_diagnostic,
    error::DocumentError,
    hover::{HoverFinder, HoverInfo},
};
use analyzer::{Module, ModuleGraph};
use ast::ASTVisitorNoArgs;
use tower_lsp::lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    FullDocumentDiagnosticReport, HoverParams, Position, RelatedFullDocumentDiagnosticReport, Url,
};
use utils::get_parent_dir;

#[derive(Debug)]
pub struct DocumentManager {
    pub graphs: RwLock<Vec<ModuleGraph>>,
}

impl DocumentManager {
    pub fn new() -> Self {
        let manager = DocumentManager {
            graphs: RwLock::new(vec![]),
        };
        manager
    }
    /// Add a new document to be tracked.
    pub fn add_document(&self, params: DidOpenTextDocumentParams) -> Result<(), DocumentError> {
        let mut graphs = self.graphs.write().unwrap();
        let uri = params.text_document.uri;
        let path_buf = uri_to_absolute_path(uri)?;
        let parent_folder = get_parent_dir(&path_buf)
            .ok_or_else(|| DocumentError::NoParentFolder(path_buf.clone()))?;

        // Containing folder is already part of a module graph.
        for graph in graphs.iter_mut() {
            if graph.contains_folder(parent_folder) {
                match Module::from_path(path_buf, graph.len()) {
                    Ok(module) => {
                        graph.add_module(module);
                        graph.unravel();
                    }
                    Err(error) => {
                        graph.add_error(error);
                    }
                }
                return Ok(());
            }
        }
        // No graph has the file. Add a new graph.
        let mut graph = ModuleGraph::new();
        let mut root_folder = parent_folder;
        // // Look 5 levels above to try to find the root of the project.
        for _ in 0..5 {
            let children: Vec<_> = root_folder
                .read_dir()?
                .filter_map(|entry| entry.ok())
                .collect();
            //     // Find Main source module.
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
                root_folder = get_parent_dir(root_folder)
                    .ok_or_else(|| DocumentError::NoParentFolder(root_folder.to_path_buf()))?;
                continue;
            }
            // Check again to see if there is already a graph with this module.
            for graph in graphs.iter_mut() {
                if graph.contains_folder(root_folder) {
                    match Module::from_path(path_buf, graph.len()) {
                        Ok(module) => {
                            graph.add_module(module);
                            graph.unravel();
                        }
                        Err(error) => {
                            graph.add_error(error);
                        }
                    }
                    return Ok(());
                }
            }
            graph.set_entry_module(main_module.unwrap());
            // Todo: read whirl.yaml to find source module instead.
            break;
        }
        // Root not found, skip project altogether.
        if graph.is_empty() {
            return Err(DocumentError::CouldNotDetermineMain);
        }
        graph.unravel();
        graphs.push(graph);
        Ok(())
    }

    /// Hover over support.
    pub fn get_hover_info(&self, params: HoverParams) -> Option<HoverInfo> {
        let uri = params.text_document_position_params.text_document.uri;
        let path_buf = uri_to_absolute_path(uri).ok()?;
        let graphs = self.graphs.read().unwrap();
        let main_graph = graphs.iter().find(|graph| graph.contains_file(&path_buf))?;
        let module = main_graph.get_module_at_path(&path_buf)?;

        get_hover_for_position(
            module,
            main_graph,
            params.text_document_position_params.position,
        )
    }
    /// Checks if a uri is already being tracked.
    pub fn has(&self, uri: Url) -> bool {
        let graphs = self.graphs.read().unwrap();
        let path_buf = match uri_to_absolute_path(uri) {
            Ok(path_buf) => path_buf,
            Err(_) => return false,
        };
        for graph in graphs.iter() {
            if graph.contains_file(&path_buf) {
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

    /// Handles didChange event
    pub fn handle_change(&self, mut params: DidChangeTextDocumentParams) -> Option<()> {
        let uri = params.text_document.uri;
        let path_buf = uri_to_absolute_path(uri).ok()?;
        let mut graphs = self.graphs.write().unwrap();
        let graph = graphs
            .iter_mut()
            .find(|graph| graph.contains_file(&path_buf))?;
        let last = params.content_changes.len() - 1;
        let most_current = std::mem::take(&mut params.content_changes[last].text);
        graph
            .get_mut_module_at_path(&path_buf)?
            .refresh_with_text(most_current);
        // Audit all modules in the graph. // todo: find more performant way to update imports.
        graph.refresh();
        Some(())
    }
    /// Get diagnostics.
    pub fn get_diagnostics(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Option<DocumentDiagnosticReport> {
        let uri = params.text_document.uri;
        let path_buf = uri_to_absolute_path(uri).ok()?;
        let graphs = self.graphs.read().unwrap();
        graphs
            .iter()
            .find_map(|graph| match graph.contains_file(&path_buf) {
                true => graph.get_module_at_path(&path_buf),
                false => None,
            })
            .map(|module| {
                DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: module
                            .errors()
                            .map(|p| to_diagnostic(&p))
                            .collect::<Vec<Diagnostic>>(),
                    },
                })
            })
    }
}

/// Traverse through the document and pinpoint the position of the hover.
fn get_hover_for_position(
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
