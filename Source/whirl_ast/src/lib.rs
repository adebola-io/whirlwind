mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

/// Represents a range in the input text.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Span {
    pub start: [usize; 2],
    pub end: [usize; 2],
}
impl Span {
    /// Check that a point is contained in the span.
    pub fn contains(&self, position: [u32; 2]) -> bool {
        self.start[0] <= position[0] as usize
            && self.start[1] <= position[1] as usize
            && self.end[0] >= position[0] as usize
            && self.end[1] >= position[1] as usize
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: [1, 1],
            end: [1, 1],
        }
    }
}

impl From<[usize; 4]> for Span {
    fn from(value: [usize; 4]) -> Self {
        Span {
            start: [value[0], value[1]],
            end: [value[2], value[3]],
        }
    }
}

impl From<[[usize; 2]; 2]> for Span {
    fn from(value: [[usize; 2]; 2]) -> Self {
        Span {
            start: value[0],
            end: value[1],
        }
    }
}
