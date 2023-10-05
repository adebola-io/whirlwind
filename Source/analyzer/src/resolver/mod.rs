mod binding;
mod context;
mod module;
mod modulegraph;
mod program;
mod symbols;

pub use binding::*;
pub use context::*;
pub use module::Module;
pub use modulegraph::ModuleGraph;
use std::path::{Path, PathBuf};
pub use symbols::*;

/// Takes in a path to a Whirl source file and builds a graph of all modules it connects to.
pub fn resolve_modules<P: AsRef<Path>>(entry: P) -> ModuleGraph {
    let entry = PathBuf::from(entry.as_ref());
    let mut graph = ModuleGraph::new();
    let mut errors = vec![];
    match entry.canonicalize() {
        Ok(absolute_path) => match Module::from_path(absolute_path, 0) {
            Ok(module) => {
                graph.set_entry_module(module);
            }
            Err(error) => errors.push(error),
        },
        Err(error) => {
            errors.push(errors::error_reading_entry_file(error));
        }
    };
    graph.errors.append(&mut errors);
    graph.unravel();
    graph
}

#[cfg(test)]
mod tests {
    use crate::resolve_modules;
    use std::path::PathBuf;

    #[test]
    fn check_imports() {
        let _graph = resolve_modules(PathBuf::from("../corelib/Core/Source/Lib.wrl"));
        let mut pathway = vec![];
        _graph.draw_line_to(_graph.get_module_with_id(14).unwrap(), &mut pathway);
    }
}
