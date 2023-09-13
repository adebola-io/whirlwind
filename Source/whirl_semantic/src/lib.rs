mod module;
mod primitive;
mod typechecker;

use primitive::Primitives;
pub use typechecker::*;

pub use module::Module;

#[derive(Debug)]
pub enum ProgramError {
    ParserError(whirl_parser::ParseError),
    LexerError(whirl_lexer::LexError),
    TypeError(typechecker::TypeError),
}

/// Parses and typechecks text input.
pub fn type_check_text(input: &str) -> Typechecker<whirl_lexer::TextLexer> {
    Typechecker::<whirl_lexer::TextLexer>::new(input, Primitives::create())
}
