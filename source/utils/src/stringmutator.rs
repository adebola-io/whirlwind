use std::{ops::Range, str::Chars};

/// A mutation to a string.
pub struct StringMutation {
    range: Range<usize>,
    idx: usize,
    new_text: Vec<char>,
}

/// An boundable iterator for strings containing text changes.
/// It reads the orignal source as is, but it produces the modified characters when it reaches the range of a change.
/// It can also choose a different start and end for iteration.
pub struct StringEditor<'a> {
    chars: Chars<'a>,
    change: StringMutation,
    /// Current character in the stream.
    idx: usize,
    /// The character index at which it should stop yielding.
    end: Option<usize>,
}

impl StringMutation {
    /// Create a new text mutation.
    /// The start and end are the indexes for the range of characters that should be replaced.
    /// # Panics
    /// It panics if start is greater than end.
    pub fn new(text: &str, start: usize, end: usize) -> Self {
        if start > end {
            panic!("Cannot create mutation with invalid range {start} -> {end}");
        }
        StringMutation {
            range: Range { start, end },
            idx: 0,
            new_text: text.chars().collect(),
        }
    }

    /// Create a mutation that empties a string.
    pub fn empty_str(original_text: &str) -> Self {
        StringMutation {
            range: Range {
                start: 0,
                end: original_text.len(),
            },
            idx: 0,
            new_text: vec![],
        }
    }

    /// Returns true if an index is contained in the area in which a mutation is valid.
    pub fn touches(&self, idx: usize) -> bool {
        // In the range of the crossed out text or the range of new text.
        self.range.start <= idx
            && (self.range.end >= idx || self.new_text.len() + self.range.start > idx)
    }
}

impl Iterator for StringMutation {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.new_text.len() {
            let character = Some(self.new_text[self.idx]);
            self.idx += 1;
            character
        } else {
            None
        }
    }
}

impl<'a> StringEditor<'a> {
    /// Create a new revised text iterator from a string and a mutation.
    /// The optional bounds are indexes used to constrain the slice to iterate over.
    pub fn new(
        original_text: &'a str,
        mutation: StringMutation,
        bounds: Option<Range<usize>>,
    ) -> Self {
        let mut chars = original_text.chars();
        // Shift to bound start.
        let (idx, end) = if let Some(Range { start, end }) = bounds {
            if start > 0 {
                chars.nth(start - 1);
            }
            (start, Some(end))
        } else {
            (0, None)
        };
        StringEditor {
            chars,
            change: mutation,
            idx,
            end,
        }
    }
}

impl<'a> Iterator for StringEditor<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let change_is_in_play = self.change.touches(self.idx);
        // Constrain reviser.
        if self
            .end
            .is_some_and(|e| e + 1 == self.idx && !change_is_in_play)
        {
            return None;
        }
        let character = if change_is_in_play {
            // If the change is empty, skip to the next character.
            self.change.next().or_else(|| {
                self.idx = self.change.range.end + 1;
                // Skip to next char.
                let skip_width = self.change.range.end - self.change.range.start;
                self.chars.nth(skip_width)
            })
        } else if self.idx == self.change.new_text.len() + self.change.range.start {
            // Skip to next char.
            let skip_width = self.change.range.end - self.change.range.start;
            self.chars.nth(skip_width)
        } else {
            self.chars.next()
        };
        self.idx += 1;
        character
    }
}

#[cfg(test)]
mod tests {
    use crate::{StringEditor, StringMutation};

    #[test]
    fn test_revision() {
        let main_text = "I am a writer";
        let mutation = StringMutation::new("singer", 7, 13);

        let reviser = StringEditor::new(main_text, mutation, None);
        let new_string: String = reviser.collect();

        assert_eq!(new_string, "I am a singer");
    }

    #[test]
    fn test_revision_with_longer_text() {
        let main_text = "His*name is Charlie";
        let mutation = StringMutation::new(" middle ", 3, 4);

        let reviser = StringEditor::new(main_text, mutation, None);
        let new_string: String = reviser.collect();

        assert_eq!(new_string, "His middle name is Charlie");
    }

    #[test]
    fn test_constraining() {
        let main_text = "Hello, My name is John.";
        let mutation = StringMutation::new("Michael", 18, 22);

        let reviser = StringEditor::new(main_text, mutation, Some(7..22));
        let new_string: String = reviser.collect();

        assert_eq!(new_string, "My name is Michael.");
    }
}
