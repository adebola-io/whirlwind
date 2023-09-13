// #![allow(unused)]

pub use parser::Parser;

use whirl_lexer::{lex_text, TextLexer};

mod errors;
mod parser;
mod test;

pub use errors::ParseError;

/// Returns an iterable parser for text input.
pub fn parse_text(input: &str) -> Parser<TextLexer> {
    Parser::from_lexer(lex_text(input))
}
