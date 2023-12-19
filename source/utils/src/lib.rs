mod container;
mod fs;
mod partial;
mod stringmutator;
mod unorderedmap;

pub use container::*;
pub use fs::*;
pub use partial::*;
use std::{
    path::Path,
    sync::{Arc, Mutex},
};
pub use stringmutator::*;
pub use unorderedmap::*;

pub fn get_parent_dir(path: &Path) -> Option<&Path> {
    path.ancestors().nth(1)
}

pub type Atomic<T> = Arc<Mutex<T>>;
