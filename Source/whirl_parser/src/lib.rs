// #![allow(unused)]

use parser::Parser;

use whirl_lexer::{lex_text, TextLexer};

mod errors;
mod module;
mod parser;
mod test;

pub use module::Module;
/// Returns an iterable parser for text input.
pub fn parse_text(input: &str) -> Parser<TextLexer> {
    Parser::from_lexer(lex_text(input))
}
