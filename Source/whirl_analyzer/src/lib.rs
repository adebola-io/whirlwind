mod module;
mod primitive;
mod typechecker;

use std::str::Chars;

use primitive::Primitives;
pub use typechecker::*;

pub use module::Module;
use whirl_lexer::TextLexer;

/// Parses and typechecks text input.
pub fn type_check_text(input: &str) -> Typechecker<TextLexer<Chars>> {
    Typechecker::<TextLexer<Chars>>::from_text(input, Primitives::create())
}
