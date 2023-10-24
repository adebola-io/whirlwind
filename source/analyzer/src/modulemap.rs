use crate::TypedModule;

/// An index into the paths stored.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct PathIndex(pub u32);

#[derive(Debug)]
pub struct ModuleMap {
    entries: Vec<Entry>,
    holes: Vec<usize>,
}

#[derive(Debug, Default)]
enum Entry {
    #[default]
    Void,
    Entry(TypedModule),
}

impl ModuleMap {
    /// Create a new Path table.
    pub fn new() -> Self {
        Self {
            entries: vec![],
            holes: vec![],
        }
    }
    /// Returns an iterator over all the paths in the table and their indexes.
    pub fn paths(&self) -> impl Iterator<Item = (PathIndex, &TypedModule)> {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|entry| match entry {
                (pathidx, Entry::Entry(entry)) => Some((PathIndex(pathidx as u32), entry)),
                _ => None,
            })
    }

    /// Returns a mutable iterator over all the paths in the table and their indexes.
    pub fn paths_mut(&mut self) -> impl Iterator<Item = (PathIndex, &mut TypedModule)> {
        self.entries
            .iter_mut()
            .enumerate()
            .filter_map(|entry| match entry {
                (pathidx, Entry::Entry(entry)) => Some((PathIndex(pathidx as u32), entry)),
                _ => None,
            })
    }

    /// Reserve an index for a module to be added later.
    pub fn reserve_index(&mut self) -> PathIndex {
        match self.holes.last() {
            Some(last_hole) => PathIndex(*last_hole as u32),
            None => {
                let index_number = self.entries.len();
                self.holes.push(index_number);
                self.entries.push(Entry::Void);
                PathIndex(index_number as u32)
            }
        }
    }

    /// Add a module to the table and return its index number.
    pub fn add(&mut self, module: TypedModule) -> PathIndex {
        // Fill any holes by removed paths.
        let index = match self.holes.pop() {
            Some(void_idx) => {
                self.entries[void_idx] = Entry::Entry(module);
                void_idx
            }
            None => {
                let id = self.entries.len();
                self.entries.push(Entry::Entry(module));
                id
            }
        };
        PathIndex(index as u32)
    }
    /// Returns the module at an index.
    pub fn get(&self, index: PathIndex) -> Option<&TypedModule> {
        match self.entries.get(index.0 as usize)? {
            Entry::Void => None,
            Entry::Entry(module) => Some(module),
        }
    }
    /// Returns the module at an index.
    pub fn get_mut(&mut self, index: PathIndex) -> Option<&mut TypedModule> {
        match self.entries.get_mut(index.0 as usize)? {
            Entry::Void => None,
            Entry::Entry(module) => Some(module),
        }
    }
    /// Remove a module using its index.
    pub fn remove(&mut self, index: PathIndex) -> Option<TypedModule> {
        let symbolentry = std::mem::take(self.entries.get_mut(index.0 as usize)?);
        self.holes.push(index.0 as usize);
        match symbolentry {
            Entry::Void => None,
            Entry::Entry(module) => Some(module),
        }
    }
    /// Returns the number of symbols in the table.
    pub fn len(&self) -> usize {
        self.entries.len() - self.holes.len()
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
