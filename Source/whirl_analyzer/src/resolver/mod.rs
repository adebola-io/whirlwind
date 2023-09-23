mod module;
mod modulegraph;
mod program;

pub use module::Module;
pub use modulegraph::ModuleGraph;
use std::path::PathBuf;

/// Takes in a path to a Whirl source file and builds a graph of all modules it connects to.
pub fn resolve(entry: PathBuf) -> ModuleGraph {
    let mut graph = ModuleGraph::new();
    let mut errors = vec![];
    match entry.canonicalize() {
        Ok(absolute_path) => match Module::from_path(absolute_path, 0) {
            Ok(module) => {
                graph.add_module(module);
            }
            Err(error) => errors.push(error),
        },
        Err(error) => {
            errors.push(whirl_errors::error_reading_entry_file(error));
        }
    };
    graph.errors.append(&mut errors);
    graph.unravel();
    graph
}

#[cfg(test)]
mod tests {
    use crate::resolve;
    use std::path::PathBuf;

    #[test]
    fn check_imports() {
        let _graph = resolve(PathBuf::from(
            "../whirl_core/Core/Source/Primitives/String.wrl",
        ));
        // println!("{:#?}", _graph)
    }
}
