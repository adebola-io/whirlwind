mod error;
mod lexer_inner;
mod test;
mod token;

use error::LexError;
use lexer_inner::LexerInner;
use token::{Span, Token};

/// A lexer for tokenizing Whirl text.
pub struct TextLexer<'input> {
    chars: std::str::Chars<'input>,
    position: usize,
    line: usize,
    span_start: [usize; 2],
    errors: Vec<LexError>,
}

pub trait Lexer: LexerInner {
    /// Asynchronously lexes and provides tokens.
    fn get_next_token(&mut self) -> Option<Token> {
        self.next_token_inner()
    }
    /// Returns an array of the vectors encountered while parsing.    
    fn errors(&self) -> &Vec<LexError>;
}

impl<'input> LexerInner for TextLexer<'input> {
    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|char| {
            if char == '\n' {
                // Return the carriage to the start of a new line.
                self.position = 1;
                self.line += 1;
            } else {
                self.position += 1;
            }
            char
        })
    }

    fn start_span(&mut self, offset: usize) {
        self.span_start = [self.position - offset, self.line];
    }

    fn report_span(&self) -> Span {
        Span {
            start: self.span_start,
            end: self.current_pos(),
        }
    }

    fn current_pos(&self) -> [usize; 2] {
        [self.line, self.position]
    }

    fn add_error(&mut self, error: LexError) {
        self.errors.push(error)
    }
}

impl<'input> Lexer for TextLexer<'input> {
    fn errors(&self) -> &Vec<LexError> {
        todo!()
    }
}
/// Lexes valid code and returns a token Iterator.
pub fn lex_text(input: &str) -> TextLexer {
    TextLexer {
        chars: input.chars(),
        position: 1,
        line: 1,
        span_start: [1, 1],
        errors: vec![],
    }
}
