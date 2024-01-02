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
    TypeInInterfacePosition(ast::TypeExpression),
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
    /// Using continue outside a loop.
    ContinueOutsideLoop,
    BreakOutsideLoop,
    NumericValueInArray,
    TypeConditionExpected,
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

pub fn type_in_interface_position(r#interface: ast::TypeExpression) -> ParseError {
    ParseError {
        span: r#interface.span(),
        _type: ParserErrorType::TypeInInterfacePosition(r#interface),
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

pub fn numeric_value_in_array_type(span: Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::NumericValueInArray,
        span,
    }
}

pub fn module_declaration_not_global(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::NonGlobalModuleDeclaration,
        span,
    }
}

pub fn invalid_return(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::InvalidReturn,
        span,
    }
}

pub fn duplicate_constructor(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::DuplicateConstructor,
        span,
    }
}

pub fn empty_path_list(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::EmptyPathList,
        span,
    }
}

pub fn continue_outside_loop(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::ContinueOutsideLoop,
        span,
    }
}

pub fn break_outside_loop(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::BreakOutsideLoop,
        span,
    }
}

pub fn empty_enum_tag(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::EmptyEnumTag,
        span,
    }
}

pub fn public_in_non_global_scope(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::PublicAccessInNonGlobalScope,
        span,
    }
}

pub fn non_global_use(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::UseImportInNonGlobalScope,
        span,
    }
}

pub fn type_condition_expected(span: ast::Span) -> ParseError {
    ParseError {
        _type: ParserErrorType::TypeConditionExpected,
        span,
    }
}
