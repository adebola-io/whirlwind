mod lexer_inner;
mod test;
mod text_lexer;

use lexer_inner::LexerInner;
pub use text_lexer::{lex_text, TextLexer};
use whirl_ast::{Token, TokenType};
use whirl_errors::LexError;

pub trait Lexer: LexerInner + Iterator<Item = Token> {
    fn module_id(&self) -> usize;
    /// Lazily lexes and provides the next token in a stream.
    fn get_next_token(&mut self) -> Option<Token> {
        self.next_token_inner()
    }
    /// Bypass comments and invalid tokens in the stream and return the next syntactic token.
    fn next_useful_token(&mut self) -> Option<Token> {
        loop {
            if let Some(token) = self.next_token_inner() {
                match token._type {
                    TokenType::Comment(_) | TokenType::Invalid(_) => {}
                    _ => return Some(token),
                }
            } else {
                return None;
            }
        }
    }
    /// Returns an array of the vectors encountered while parsing.    
    fn errors(&mut self) -> &mut Vec<LexError>;
}
