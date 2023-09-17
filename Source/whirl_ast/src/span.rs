use std::ops::Range;

/// A trait that allows for AST nodes to be moved either by lines or characters.
/// It is used to edit the AST in place so that text can continue to be parsed without rebuilding.
pub trait Positioning {
    /// Adjust an AST node by moving its components vertically.
    fn move_by_line(&mut self, offset: i32);
    /// Adjust an AST node by moving its components horizontally.
    fn move_by_character(&mut self, offset: i32);
    /// Returns the individual nodes within a node that are closest to a span.
    fn closest_nodes_to(&mut self, span: Span) -> Vec<&Self>;
}

/// Represents a range in input text.
/// It consists of a start and end, which are each made up of an array of two numbers, representing the source line and character.
#[derive(PartialEq, Clone, Copy)]
pub struct Span {
    pub start: [u32; 2],
    pub end: [u32; 2],
}
impl Span {
    /// Create a span at a single point.
    pub fn at(position: [u32; 2]) -> Self {
        Self {
            start: position,
            end: position,
        }
    }

    /// Check if a span is a child of this span.
    pub fn encloses(&self, other: Span) -> bool {
        self.contains(other.start) && self.contains(other.end)
    }
    /// Check that a point is inclusively contained in the span.
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
    /// Here be literal magic.
    /// Converts a span to an `a..b` range, given the lengths of each line in the source text.
    pub fn to_range(self, line_lengths: &[usize]) -> Range<usize> {
        // minus happens because spans are one-based.
        let [start_line, start_char] = self.start.map(|x| x as usize);
        let [end_line, end_char] = self.end.map(|x| x as usize);

        let mut start = 0;
        for line_length in line_lengths[0..(start_line - 1)].into_iter() {
            // Gather all line_lengths (and the newline character) up to this start of the span.
            start += line_length + 1;
        }

        let end = if start_line == end_line {
            start + (start_char + end_char) - 2
        } else {
            let mut inner_width = 0;
            for line in start_line..end_line {
                inner_width += line_lengths[line - 1] + 1;
            }
            inner_width + start_char + end_char - 1
        };

        Range { start, end }
    }
    /// Checks that a point exists immediately after or immediately before a span.
    pub fn is_directly_adjacent_to(&self, position: [u32; 2]) -> bool {
        return (position[0] == self.start[0] && position[1] == self.start[1] - 1)
            || (position[0] == self.end[0] && position[1] == self.end[1] + 1);
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
    // use std::time::Instant;

    use crate::Span;

    #[test]
    fn testing_span_contains() {
        let span = Span::from([1, 4, 5, 5]);

        assert!(span.contains([2, 3]));
        assert!(span.contains([5, 1]));

        assert!(!span.contains([1, 1]));
        assert!(!span.contains([5, 9]))
    }

    #[test]
    fn test_adjacence() {
        let span = Span::from([1, 4, 5, 5]);

        assert!(span.is_directly_adjacent_to([1, 3]));
        assert!(span.is_directly_adjacent_to([5, 6]));

        assert!(!span.is_directly_adjacent_to([1, 10]));
        assert!(!span.is_directly_adjacent_to([5, 12]));
    }

    #[test]
    fn to_range() {
        let text = "
function Hello() {

}
";
        let line_lengths: Vec<usize> = text.lines().map(|s| s.len()).collect();
        let span = Span::from([2, 1, 2, 9]);
        let range = span.to_range(&line_lengths);
        assert_eq!(text.get(range).unwrap(), "function");

        assert_eq!(
            text.get(Span::from([2, 1, 4, 2]).to_range(&line_lengths))
                .unwrap(),
            "function Hello() {

}"
        )
    }
}
