mod container;
mod fs;
mod partial;
mod stringmutator;

use std::path::Path;

pub use container::*;
pub use fs::*;
pub use partial::*;
pub use stringmutator::*;

pub fn get_parent_dir(path: &Path) -> Option<&Path> {
    path.ancestors().nth(1)
}
