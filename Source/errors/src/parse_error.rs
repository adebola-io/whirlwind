use ast::{Span, TokenType};

#[derive(Debug, Default, PartialEq)]
pub struct ParseError {
    pub _type: ParserErrorType,
    pub span: Span,
}

#[derive(Debug, Default, PartialEq)]
pub enum ParserErrorType {
    DeclarationOrStatementExpected,
    PublicShorthandVariable,
    IdentifierExpected,
    DeclarationExpected,
    Expected(TokenType),
    AsyncType,
    PublicAccessTypeOnTest,
    GenericArgsInNamespace,
    #[default]
    UnexpectedToken,
    StringExpected,
    ExpressionExpected,
    TypeInTraitPosition(ast::TypeExpression),
    ExpectedAttribute,
    PublicAccessTypeOnConstructor,
    DuplicateModuleName,
    NonGlobalModuleDeclaration,
    InvalidReturn,
    DuplicateConstructor,
    EmptyPathList,
    UseImportInNonGlobalScope,
    PublicAccessInNonGlobalScope,
    /// Writing a test in a local scope.
    TestInNonGlobalScope,
    /// Global control flow statements.
    GlobalControl,
    /// Writing a enum variant with an empty list of tagged types.
    EmptyEnumTag,
}

pub fn public_shorthand_var(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::PublicShorthandVariable,
        span,
    }
}

pub fn declaration_expected(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::DeclarationExpected,
        span,
    }
}

pub fn declaration_or_statement_expected(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::DeclarationOrStatementExpected,
        span,
    }
}

pub fn identifier_expected(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::IdentifierExpected,
        span,
    }
}

pub fn expected(token_type: TokenType, span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::Expected(token_type),
        span,
    }
}

pub fn public_test(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::PublicAccessTypeOnTest,
        span,
    }
}

pub fn generic_args_in_namespace(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::GenericArgsInNamespace,
        span,
    }
}

pub fn unexpected(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::UnexpectedToken,
        span,
    }
}

pub fn async_type(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::AsyncType,
        span,
    }
}

pub fn string_expected(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::StringExpected,
        span,
    }
}

pub fn expression_expected(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::ExpressionExpected,
        span,
    }
}

pub fn type_in_trait_position(r#trait: ast::TypeExpression) -> ParseError {
    ParseError {
        span: r#trait.span(),
        _type: ParserErrorType::TypeInTraitPosition(r#trait),
    }
}

pub fn expected_attribute(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::ExpectedAttribute,
        span,
    }
}

pub fn public_on_new(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::PublicAccessTypeOnConstructor,
        span,
    }
}

pub fn duplicate_module_name(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::DuplicateModuleName,
        span,
    }
}

pub fn global_control(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::GlobalControl,
        span,
    }
}

pub fn test_in_non_global_scope(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::TestInNonGlobalScope,
        span,
    }
}
