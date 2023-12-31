mod container;
mod fs;
mod partial;
mod stringmutator;
mod threadpool;
mod unorderedmap;

pub use container::*;
pub use fs::*;
pub use partial::*;
use std::{
    path::Path,
    sync::{Arc, Mutex},
};
pub use stringmutator::*;
pub mod terminal;
pub use threadpool::*;
pub use unorderedmap::*;

pub fn get_parent_dir(path: &Path) -> Option<&Path> {
    path.ancestors().nth(1)
}

pub fn get_dir_basename(path: &std::path::Path) -> Option<&str> {
    path.components()
        .last()
        .map(|component| component.as_os_str())
        .and_then(|dirname| dirname.to_str())
}

pub type Atomic<T> = Arc<Mutex<T>>;
