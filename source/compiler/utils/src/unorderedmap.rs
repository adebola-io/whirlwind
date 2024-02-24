use std::collections::HashMap;

/// A very naive implementation of a hashmap that stores values
/// by assigning and reassigning vector indices.
/// It is useful for special operations that require quick and frequent
/// insertion, retrieval and deletion, but no sequential traversing.
/// A good example is the symbol table.
#[derive(Debug, Default)]
pub struct UnorderedMap<T> {
    values: Vec<MapEntry<T>>,
    holes: Vec<usize>,
}

#[derive(Debug, Default)]
enum MapEntry<Value> {
    Value(Value),
    /// A "hole" in the map.
    /// It represents an item that was previously removed.
    #[default]
    Nothing,
}

impl<T> UnorderedMap<T> {
    /// Creates a new unordered map.
    pub fn new() -> Self {
        Self {
            values: vec![],
            holes: vec![],
        }
    }
    /// Returns the number of items in the map.
    pub fn len(&self) -> usize {
        self.values.len() - self.holes.len()
    }
    /// Adds a value to the map, and returns the index at which it was added.
    /// Indexes can be sequential or not, based on whether removals have occured.
    pub fn insert(&mut self, value: T) -> usize {
        match self.holes.pop() {
            Some(index) => {
                self.values[index] = MapEntry::Value(value);
                return index;
            }
            None => {
                let index = self.values.len();
                self.values.push(MapEntry::Value(value));
                return index;
            }
        }
    }
    /// Removes the value at a particular index and returns it,
    /// if that value exists. This operation is effectively O(1),
    /// since the element is replaced with a void "hole" that takes up
    /// the same space.
    pub fn remove(&mut self, index: usize) -> Option<T> {
        let entry = std::mem::take(self.values.get_mut(index)?);
        match entry {
            MapEntry::Nothing => None, // The value was already removed.
            MapEntry::Value(value) => {
                self.holes.push(index);
                Some(value)
            }
        }
    }
    /// Returns a reference to the item at a particular index,
    /// if that value exists.
    pub fn get(&self, index: usize) -> Option<&T> {
        match self.values.get(index)? {
            MapEntry::Value(value) => Some(value),
            MapEntry::Nothing => None,
        }
    }
    /// Returns a mutable reference to the value at a particular index.
    /// if that value exists.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        match self.values.get_mut(index)? {
            MapEntry::Value(value) => Some(value),
            MapEntry::Nothing => None,
        }
    }
    /// Returns an iterator over the values in the map.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.values.iter().filter_map(|entry| match entry {
            MapEntry::Value(value) => Some(value),
            MapEntry::Nothing => None,
        })
    }
    /// Returns a mutable iterator over the values in the map.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut().filter_map(|entry| match entry {
            MapEntry::Value(value) => Some(value),
            MapEntry::Nothing => None,
        })
    }
    /// Reserve an index for a value to be added later.
    pub fn reserve(&mut self) -> usize {
        match self.holes.last() {
            Some(last_hole) => *last_hole,
            None => {
                let index = self.values.len();
                self.holes.push(index);
                self.values.push(MapEntry::Nothing);
                index
            }
        }
    }
    /// Removes all holes in the map.
    /// It returns a mapping of old indices to new ones
    /// for the values contained.
    pub fn shrink(&mut self) -> HashMap<usize, usize> {
        let map = HashMap::new();
        for hole in &self.holes {
            self.values.swap_remove(*hole);
        }
        self.holes = vec![];
        map
    }
}

#[cfg(test)]
mod tests {
    use crate::UnorderedMap;

    #[test]
    fn test_unordered_map() {
        let mut map = UnorderedMap::new();
        // Insertion.
        let index_of_hello = map.insert("hello");
        let index_of_world = map.insert("world");

        // Deletion
        map.remove(index_of_hello);
        assert!(map.iter().find(|value| **value == "hello").is_none());
        map.remove(index_of_world);
        assert!(map.iter().find(|value| **value == "world").is_none());

        assert_eq!(map.len(), 0)
    }
}
