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

pub fn trait_as_type(name: &str, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TraitAsType {
            name: name.to_owned(),
        },
        spans: vec![span],
    }
}
