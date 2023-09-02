use crate::token::Span;

pub struct LexError {
    pub error_type: LexErrorType,
    pub position: LexErrorPos,
}

/// The point to mark in an error.
pub enum LexErrorPos {
    Point([usize; 2]),
    Span(Span),
}

pub enum LexErrorType {
    UnexpectedEndOfInput,
    UnterminatedString,
}
