use whirl_ast::{Span, TokenType};

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub error_type: ParserErrorType,
    pub span: Span,
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
    TypeInTraitPosition(whirl_ast::TypeExpression),
    ExpectedAttribute,
    PublicAccessTypeOnConstructor,
    DuplicateModuleName,
    NonGlobalModuleDeclaration,
    InvalidReturn,
    DuplicateConstructor,
    EmptyPathList,
}

pub fn public_shorthand_var(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::PublicShorthandVariable,
        span,
    }
}

pub fn declaration_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DeclarationExpected,
        span,
    }
}

pub fn declaration_or_statement_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DeclarationOrStatementExpected,
        span,
    }
}

pub fn identifier_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::IdentifierExpected,
        span,
    }
}

pub fn expected(token_type: TokenType, span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::Expected(token_type),
        span,
    }
}

pub fn public_test(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::PublicAccessTypeOnTest,
        span,
    }
}

pub fn generic_args_in_namespace(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::GenericArgsInNamespace,
        span,
    }
}

pub fn unexpected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::UnexpectedToken,
        span,
    }
}

pub fn async_type(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::AsyncType,
        span,
    }
}

pub fn string_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::StringExpected,
        span,
    }
}

pub fn expression_expected(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::ExpressionExpected,
        span,
    }
}

pub fn type_in_trait_position(r#trait: whirl_ast::TypeExpression) -> ParseError {
    ParseError {
        span: r#trait.span(),
        error_type: ParserErrorType::TypeInTraitPosition(r#trait),
    }
}

pub fn expected_attribute(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::ExpectedAttribute,
        span,
    }
}

pub fn public_on_new(span: Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::PublicAccessTypeOnConstructor,
        span,
    }
}

pub fn duplicate_module_name(span: whirl_ast::Span) -> ParseError {
    ParseError {
        error_type: ParserErrorType::DuplicateModuleName,
        span,
    }
}
