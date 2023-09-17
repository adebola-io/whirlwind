// #![allow(unused)]

use std::str::Chars;

pub use parser::Parser;

use whirl_lexer::{lex_text, TextLexer};

mod parser;
mod test;

/// Returns an iterable parser for text input.
pub fn parse_text(input: &str) -> Parser<TextLexer<Chars>> {
    Parser::from_lexer(lex_text(input))
}
