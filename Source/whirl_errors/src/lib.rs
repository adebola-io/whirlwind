mod lex_error;
mod parse_error;
mod type_error;

pub use lex_error::*;
pub use parse_error::*;
pub use type_error::*;

#[derive(Debug)]
pub enum ProgramError {
    ParserError(ParseError),
    LexerError(LexError),
    TypeError(TypeError),
}
