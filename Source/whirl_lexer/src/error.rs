use whirl_ast::Span;

#[derive(Debug)]
pub struct LexError {
    pub error_type: LexErrorType,
    pub position: LexErrorPos,
}

#[derive(Debug)]
/// The point to mark in an error.
pub enum LexErrorPos {
    Point([u32; 2]),
    Span(Span),
}

#[derive(Debug)]
pub enum LexErrorType {
    UnexpectedEndOfInput,
    UnterminatedString,
    InvalidCharacter(char),
}
