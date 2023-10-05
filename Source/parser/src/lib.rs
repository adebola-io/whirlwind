// #![allow(unused)]

use std::str::Chars;

pub use parse::Parser;

use lexer::{lex_text, TextLexer};

mod parse;
mod test;

/// Returns an iterable parser for text input.
pub fn parse_text(input: &str) -> Parser<TextLexer<Chars>> {
    Parser::from_lexer(lex_text(input))
}
