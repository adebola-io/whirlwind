use ast::Span;

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub error_type: LexErrorType,
    pub position: LexErrorPos,
}
impl LexError {
    pub fn unterminated_string(position: LexErrorPos) -> LexError {
        LexError {
            error_type: LexErrorType::UnterminatedString,
            position,
        }
    }

    pub fn no_value_after_exponent(position: LexErrorPos) -> LexError {
        LexError {
            error_type: LexErrorType::NoValAfterExponent,
            position,
        }
    }
}

#[derive(Debug, PartialEq)]
/// The point to mark in an error.
pub enum LexErrorPos {
    Point([u32; 2]),
    Span(Span),
}

#[derive(Debug, PartialEq)]
pub enum LexErrorType {
    UnexpectedEndOfInput,
    UnterminatedString,
    InvalidCharacter(char),
    NoValAfterExponent,
    ExponentforInvalidBase,
}
