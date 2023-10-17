use std::str::Chars;

use ast::{Span, Token};
use errors::LexError;

use crate::{lexer_inner::LexerInner, Lexer};

/// A generic lexer for Whirlwind text.
pub struct TextLexer<I: Iterator<Item = char>> {
    chars: I,
    position: u32,
    line: u32,
    span_start: [u32; 2],
    errors: Vec<LexError>,
    stash: Option<char>,
    /// A vector containing the lengths of each line in the string.
    /// It is useful for recomputing the number of characters in the string.
    /// It does not contain the terminators at the end of the line.
    pub line_lengths: Vec<u32>,
    module_id: usize,
    saved: Option<Token>,
}

impl<I: Iterator<Item = char>> From<I> for TextLexer<I> {
    fn from(value: I) -> Self {
        TextLexer {
            module_id: 0,
            chars: value,
            position: 1,
            line: 1,
            span_start: [1, 1],
            errors: vec![],
            stash: None,
            line_lengths: vec![],
            saved: None,
        }
    }
}

impl<I: Iterator<Item = char>> LexerInner for TextLexer<I> {
    fn next_char(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            if ch == '\n' {
                // Store line length and move to next.
                self.line_lengths.push(self.position - 1);
                self.position = 1;
                self.line += 1;
            } else {
                self.position += 1;
            }
            Some(ch)
        } else {
            // Store the length of the last line.
            self.line_lengths.push(self.position - 1);
            None
        }
    }

    fn start_span(&mut self) {
        self.span_start = [self.line, self.position - 1];
    }

    fn report_span(&self) -> Span {
        Span {
            start: self.span_start,
            end: self.current_pos(),
        }
    }

    fn current_pos(&self) -> [u32; 2] {
        [self.line, self.position]
    }

    fn add_error(&mut self, error: LexError) {
        self.errors.push(error)
    }

    fn remove_stashed(&mut self) -> Option<char> {
        self.stash.take()
    }

    fn stash(&mut self, ch: char) {
        self.stash = Some(ch)
    }

    fn line_lengths(&self) -> &[u32] {
        &self.line_lengths
    }

    fn save_token(&mut self, token: Token) {
        self.saved = Some(token)
    }

    fn remove_saved(&mut self) -> Option<Token> {
        self.saved.take()
    }
}

impl<I: Iterator<Item = char>> Lexer for TextLexer<I> {
    fn errors(&mut self) -> &mut Vec<LexError> {
        &mut self.errors
    }

    fn module_id(&self) -> usize {
        self.module_id
    }
}

impl<I: Iterator<Item = char>> Iterator for TextLexer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_next_token()
    }
}
/// Lexes valid code and returns a token Iterator.
pub fn lex_text(input: &str) -> TextLexer<Chars> {
    TextLexer {
        chars: input.chars(),
        position: 1,
        line: 1,
        span_start: [1, 1],
        errors: vec![],
        stash: None,
        line_lengths: vec![],
        saved: None,
        module_id: 0,
    }
}
