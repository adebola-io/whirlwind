use crate::Literal;
use utils::UnorderedMap;

/// An index into the list of literals in the program.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LiteralIndex(pub usize);

#[derive(Debug)]
pub struct LiteralMap {
    values: UnorderedMap<Literal>,
}

impl LiteralMap {
    /// Create a new literal map.
    pub fn new() -> Self {
        Self {
            values: UnorderedMap::new(),
        }
    }
    /// Returns an iterator over all the literals in the table and their indexes.
    pub fn literals(&self) -> impl Iterator<Item = (LiteralIndex, &Literal)> {
        self.values
            .iter()
            .enumerate()
            .map(|(idx, literal)| (LiteralIndex(idx), literal))
    }

    /// Returns a mutable iterator over all the literals in the table and their indexes.
    pub fn literals_mut(&mut self) -> impl Iterator<Item = (LiteralIndex, &mut Literal)> {
        self.values
            .iter_mut()
            .enumerate()
            .map(|(idx, literal)| (LiteralIndex(idx), literal))
    }

    /// Reserve an index for a literal to be added later.
    pub fn reserve_index(&mut self) -> LiteralIndex {
        LiteralIndex(self.values.reserve())
    }

    /// Add a literal to the table and return its index number.
    pub fn add(&mut self, literal: Literal) -> LiteralIndex {
        LiteralIndex(self.values.insert(literal))
    }
    /// Returns the literal at an index.
    pub fn get(&self, index: LiteralIndex) -> Option<&Literal> {
        self.values.get(index.0)
    }
    /// Returns the literal at an index.
    pub fn get_mut(&mut self, index: LiteralIndex) -> Option<&mut Literal> {
        self.values.get_mut(index.0)
    }
    /// Remove a literal using its index.
    pub fn remove(&mut self, index: LiteralIndex) -> Option<Literal> {
        self.values.remove(index.0)
    }
    /// Returns the number of literals in the table.
    pub fn len(&self) -> usize {
        self.values.len()
    }
}
