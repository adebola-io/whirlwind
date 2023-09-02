use crate::token::Span;

pub struct LexError {
    pub error_type: LexErrorType,
    pub position: LexErrorPos,
}

pub enum LexErrorPos {
    Point([usize; 2]),
    Span(Span),
}

pub enum LexErrorType {
    UnexpectedEndOfInput,
    UnterminatedString,
}
