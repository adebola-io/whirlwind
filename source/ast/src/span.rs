use std::ops::{Add, Range};

/// A trait that allows for AST nodes to be moved either by lines or characters.
/// It is used to edit the AST in place so that text can continue to be parsed without rebuilding.
pub trait Positioning: Spannable {
    /// Adjust an AST node by moving its components vertically.
    fn move_by_line(&mut self, offset: i32);
    /// Adjust an AST node by moving its components horizontally.
    fn move_by_character(&mut self, offset: i32);
    /// Returns the individual nodes within a node that are closest to a span.
    fn closest_nodes_to(&self, span: Span) -> Vec<&Self>;
}

/// A node that occupies a span.
pub trait Spannable {
    /// Returns the span of the node.
    fn span(&self) -> Span;
    /// Change the start position of the node.
    fn set_start(&mut self, start: [u32; 2]);
    /// Returns all the scopes contained _within_ the span of this node.
    fn captured_scopes(&self) -> Vec<usize>;
}

/// Represents a range in input text.
/// It consists of a start and end, which are each made up of an array of two numbers, representing the source line and character.
#[derive(PartialEq, Clone, Copy, Hash, Eq)]
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
    /// Create a span, given the starting line and character, and the horizontal width of the span.
    pub fn on_line(start: [u32; 2], width: u32) -> Self {
        Span {
            start,
            end: [start[0], start[1] + width],
        }
    }

    /// Returns true if the span ranges over multiple lines.
    pub fn is_multiline(&self) -> bool {
        self.start[0] != self.end[0]
    }
    /// Returns true if the span ranges over a single line.
    pub fn is_single_line(&self) -> bool {
        self.start[0] == self.end[0]
    }

    /// Returns a new span that starts at the first character of the starting line.
    pub fn from_line_start(&self) -> Span {
        Span {
            start: [self.start[0], 1],
            end: self.end,
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
    // Here be literal magic.
    /// Converts a span to an `a..b` range, given the lengths of each line in the source text.
    pub fn to_range(self, line_lengths: &[u32]) -> Range<usize> {
        // minuses happen because spans are one-based.
        let [start_line, start_char] = self.start.map(|s| s - 1);
        let [end_line, end_char] = self.end.map(|s| s - 1);

        let mut start = 0;
        let mut index = 0;
        while index < start_line {
            // Gather all line_lengths (and the newline character) up to this start of the span.
            start += line_lengths[index as usize] + 1;
            index += 1;
        }
        // Add the characters up to the start of the span.
        start += start_char;

        let end = if start_line == end_line {
            start + (end_char - start_char)
        } else {
            let mut inner_width = start - start_char;
            for line in start_line..end_line {
                let length = line_lengths[line as usize] + 1;
                inner_width += length;
            }
            inner_width + end_char
        };

        Range {
            start: start as usize,
            end: end as usize,
        }
    }

    pub fn ends_directly_before(&self, position: [u32; 2]) -> bool {
        position[0] == self.end[0] && position[1] == self.end[1] - 1
    }

    /// Checks that a point exists immediately after or immediately before a span.
    pub fn is_adjacent_to(&self, position: [u32; 2]) -> bool {
        return (position[0] == self.start[0] && position[1] == self.start[1] - 1)
            || (position[0] == self.end[0] && position[1] == self.end[1] + 1);
    }

    /// Checks that another span is directly next to or within the span.
    pub fn is_in_vicinity(&self, span: Span) -> bool {
        self.encloses(span) || self.is_adjacent_to(span.start) || self.is_adjacent_to(span.end)
    }
    /// Checks if a span exists before another.
    pub fn is_before(&self, other: Span) -> bool {
        // other is inside this span.
        if self.encloses(other) {
            return false;
        }
        if self.start[0] == other.start[0] {
            if self.start[1] < other.start[1] {
                // ending on the same line.
                if self.end[0] == other.end[0] {
                    return self.end[1] < other.end[1];
                } else {
                    return self.end[0] < other.end[0];
                }
            }
        } else if self.start[0] < other.start[0] {
            // ending on the same line.
            if self.end[0] == other.end[0] {
                return self.end[1] < other.end[1];
            } else {
                return self.end[0] < other.end[0];
            }
        }
        return false;
    }

    /// Checks if a span exists after another.
    pub fn is_after(&self, other: Span) -> bool {
        !self.is_before(other) && !self.contains(other.start) && !self.contains(other.end)
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ {:?}, {:?} }}", self.start, self.end)
    }
}

impl Add for Span {
    type Output = Self;

    /// Combine two spans to produce a span that encloses both.
    fn add(self, rhs: Self) -> Self::Output {
        let start_line = self.start[0].min(rhs.start[0]);
        let start_char = if self.start[0] == rhs.start[0] {
            self.start[1].min(rhs.start[1])
        } else if self.start[0] < rhs.start[0] {
            self.start[1]
        } else {
            rhs.start[1]
        };

        let end_line = self.end[0].max(rhs.end[0]);
        let end_char = if self.end[0] == rhs.end[0] {
            self.end[1].max(rhs.end[1])
        } else if self.end[0] > rhs.end[0] {
            self.end[1]
        } else {
            rhs.end[1]
        };

        Span {
            start: [start_line, start_char],
            end: [end_line, end_char],
        }
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

        assert!(span.is_adjacent_to([1, 3]));
        assert!(span.is_adjacent_to([5, 6]));

        assert!(!span.is_adjacent_to([1, 10]));
        assert!(!span.is_adjacent_to([5, 12]));
    }

    #[test]
    fn test_positioning() {
        let span_1 = Span::from([1, 1, 1, 5]);
        let span_2 = Span::from([1, 6, 1, 8]);

        assert!(span_1.is_before(span_2));

        let span_3 = Span::from([2, 1, 3, 5]);
        let span_4 = Span::from([4, 1, 8, 2]);

        assert!(span_3.is_before(span_4));
    }

    #[test]
    fn to_range() {
        let text = "
function Hello() {

}
";
        let line_lengths: Vec<u32> = text.lines().map(|s| s.len()).map(|x| x as u32).collect();
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

    #[test]
    fn span_arithmetic() {
        let span_1 = Span::from([1, 1, 5, 9]);
        let span_2 = Span::from([2, 4, 3, 8]);
        // span 1 is larger.
        assert_eq!(span_1 + span_2, span_1);

        let span_3 = Span::from([1, 1, 1, 99]);
        let span_4 = Span::from([2, 5, 8, 19]);
        // span 1 is larger.
        assert_eq!(span_3 + span_4, Span::from([1, 1, 8, 19]));
    }
}
