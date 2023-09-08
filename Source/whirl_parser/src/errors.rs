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
    AsyncType,
    PublicAccessTypeOnTest,
    GenericArgsInNamespace,
    UnexpectedToken,
    StringExpected,
    ExpressionExpected,
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

pub(crate) fn public_test(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::PublicAccessTypeOnTest,
        span,
    }
}

pub(crate) fn generic_args_in_namespace(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::GenericArgsInNamespace,
        span,
    }
}

pub(crate) fn unexpected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::UnexpectedToken,
        span,
    }
}

pub(crate) fn async_type(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::AsyncType,
        span,
    }
}

pub(crate) fn string_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::StringExpected,
        span,
    }
}

pub(crate) fn expression_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::ExpressionExpected,
        span,
    }
}
