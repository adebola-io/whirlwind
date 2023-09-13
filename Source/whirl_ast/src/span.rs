/// Represents a range in the input text.
#[derive(PartialEq, Clone, Copy)]
pub struct Span {
    pub start: [u32; 2],
    pub end: [u32; 2],
}
impl Span {
    /// Check that a point is contained in the span.
    pub fn contains(&self, position: [u32; 2]) -> bool {
        // Out of line boundaries.
        if position[0] < self.start[0] || position[0] > self.end[0] {
            return false;
        }
        // Same line but out of character boundaries.
        if (position[0] == self.start[0] && position[1] < self.start[1])
            || (position[0] == self.end[0] && position[1] > self.end[1])
        {
            return false;
        }
        return true;
    }
    /// Create a span at a single point.
    pub fn at(position: [u32; 2]) -> Self {
        Self {
            start: position,
            end: position,
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ {:?}, {:?} }}", self.start, self.end)
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

impl From<[u32; 4]> for Span {
    fn from(value: [u32; 4]) -> Self {
        Span {
            start: [value[0], value[1]],
            end: [value[2], value[3]],
        }
    }
}

impl From<[[u32; 2]; 2]> for Span {
    fn from(value: [[u32; 2]; 2]) -> Self {
        Span {
            start: value[0],
            end: value[1],
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Span;

    #[test]
    fn testing_span_contains() {
        let span = Span::from([1, 4, 5, 5]);

        assert!(!span.contains([1, 1]));
        assert!(span.contains([2, 3]));
        assert!(span.contains([5, 1]));
        assert!(!span.contains([5, 9]))
    }
}
