mod error;
mod lexer_inner;
mod test;
mod token;

pub use error::LexError;
use lexer_inner::LexerInner;
pub use token::*;
use whirl_ast::Span;

/// A lexer for tokenizing Whirl text.
pub struct TextLexer<'input> {
    chars: std::str::Chars<'input>,
    position: usize,
    line: usize,
    span_start: [usize; 2],
    errors: Vec<LexError>,
    stash: Option<char>,
}

pub trait Lexer: LexerInner + Iterator<Item = Token> {
    /// Asynchronously lexes and provides the next token in a stream.
    fn get_next_token(&mut self) -> Option<Token> {
        self.next_token_inner()
    }
    /// Bypass comments and invalid tokens in the stream and return the next syntactic token.
    fn next_useful_token(&mut self) -> Option<Token> {
        loop {
            match self.next_token_inner() {
                Some(token) => match token._type {
                    TokenType::Comment(_) | TokenType::Invalid(_) => {}
                    _ => return Some(token),
                },
                None => return None,
            }
        }
    }
    /// Returns an array of the vectors encountered while parsing.    
    fn errors(&mut self) -> &mut Vec<LexError>;
}

impl LexerInner for TextLexer<'_> {
    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|char| {
            if char == '\n' {
                self.position = 1;
                self.line += 1;
            } else {
                self.position += 1;
            }
            char
        })
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

    fn current_pos(&self) -> [usize; 2] {
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
}

impl Lexer for TextLexer<'_> {
    fn errors(&mut self) -> &mut Vec<LexError> {
        &mut self.errors
    }
}

impl Iterator for TextLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_next_token()
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
        stash: None,
    }
}
