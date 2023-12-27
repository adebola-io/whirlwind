use crate::TypedModule;
use utils::UnorderedMap;

/// An index into the paths stored.
#[derive(Debug, PartialEq, PartialOrd, Ord, Clone, Copy, Hash, Eq)]
pub struct PathIndex(pub u32);

#[derive(Debug)]
pub struct ModuleMap {
    values: UnorderedMap<TypedModule>,
}

impl ModuleMap {
    /// Create a new Path table.
    pub fn new() -> Self {
        Self {
            values: UnorderedMap::new(),
        }
    }
    /// Returns an iterator over all the paths in the table and their indexes.
    pub fn paths(&self) -> impl Iterator<Item = (PathIndex, &TypedModule)> {
        self.values
            .iter()
            .enumerate()
            .map(|(idx, module)| (PathIndex(idx as u32), module))
    }

    /// Returns a mutable iterator over all the paths in the table and their indexes.
    pub fn paths_mut(&mut self) -> impl Iterator<Item = (PathIndex, &mut TypedModule)> {
        self.values
            .iter_mut()
            .enumerate()
            .map(|(idx, module)| (PathIndex(idx as u32), module))
    }

    /// Reserve an index for a module to be added later.
    pub fn reserve_index(&mut self) -> PathIndex {
        PathIndex(self.values.reserve() as u32)
    }

    /// Add a module to the table and return its index number.
    pub fn add(&mut self, module: TypedModule) -> PathIndex {
        PathIndex(self.values.insert(module) as u32)
    }
    /// Returns the module at an index.
    pub fn get(&self, index: PathIndex) -> Option<&TypedModule> {
        self.values.get(index.0 as usize)
    }
    /// Returns the module at an index.
    pub fn get_mut(&mut self, index: PathIndex) -> Option<&mut TypedModule> {
        self.values.get_mut(index.0 as usize)
    }
    /// Remove a module using its index.
    pub fn remove(&mut self, index: PathIndex) -> Option<TypedModule> {
        self.values.remove(index.0 as usize)
    }
    /// Returns the number of modules in the table.
    pub fn len(&self) -> usize {
        self.values.len()
    }
    /// Returns true if a module is contained in the table.
    pub fn has(&self, module: &TypedModule) -> bool {
        self.get(module.path_idx).is_some()
    }
    /// Find the index for a path.
    pub fn map_path_to_index(&self, path: &std::path::PathBuf) -> Option<PathIndex> {
        Some(self.paths().find(|tuple| tuple.1.path_buf == *path)?.0)
    }
}
