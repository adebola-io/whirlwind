use whirl_ast::Span;
use whirl_lexer::{LexError, TokenType};

#[derive(Debug, PartialEq)]
pub struct ParseError {
    error_type: ParserErrorType,
    span: Span,
}

#[derive(Debug)]
pub enum ProgramError {
    ParserError(ParseError),
    LexerError(LexError),
}

#[derive(Debug, PartialEq)]
pub enum ParserErrorType {
    DeclarationOrStatementExpected,
    PublicShorthandVariable,
    IdentifierExpected,
    DeclarationExpected,
    Expected(TokenType),
}

pub(crate) fn public_shorthand_var(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::PublicShorthandVariable,
        span,
    }
}

pub(crate) fn declaration_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DeclarationExpected,
        span,
    }
}

pub(crate) fn declaration_or_statement_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DeclarationOrStatementExpected,
        span,
    }
}

pub(crate) fn identifier_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::IdentifierExpected,
        span,
    }
}

pub(crate) fn expected(token_type: TokenType, span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::Expected(token_type),
        span,
    }
}

pub(crate) fn expected_after(token_type: TokenType, span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::Expected(token_type),
        span: Span {
            start: span.end,
            end: span.end,
        },
    }
}
