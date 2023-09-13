use whirl_ast::ScopeManager;
use whirl_errors::{LexErrorType, ParserErrorType, TypeErrorType};

use crate::stringify_type_eval;

/// Stringify a type error.
pub fn stringify_type_error(scope_manager: &ScopeManager, error: &TypeErrorType) -> String {
    match error {
        TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        } => format!(
            "Operator '{:?}' is not defined for '{}' and '{}'.",
            operator,
            stringify_type_eval(scope_manager, &left),
            stringify_type_eval(scope_manager, &right)
        ),
        TypeErrorType::AssignedInvalid => {
            format!("The invalid type cannot be used as an expression.")
        }
        TypeErrorType::UnknownType { name } => {
            format!("Cannot resolve type '{name}'")
        }
        TypeErrorType::ValueAsType { name } => {
            format!("'{name}' refers to a value, but it is being used as a type here.")
        }
        TypeErrorType::UnexpectedGenericArgs { value } => format!("Type '{value}' is not generic."),
        TypeErrorType::MismatchedGenericArgs {
            value,
            expected,
            assigned,
        } => format!("'{value}' expects {expected} generic arguments, but got only {assigned}."),
        TypeErrorType::MismatchedAssignment { left, right } => format!(
            "Cannot assign '{}' to '{}'.",
            stringify_type_eval(scope_manager, &right),
            stringify_type_eval(scope_manager, &left),
        ),
    }
}

pub fn stringify_parse_error(error: &ParserErrorType) -> String {
    match error {
        ParserErrorType::DeclarationOrStatementExpected => {
            format!("Declaration or statement expected.")
        }
        ParserErrorType::PublicShorthandVariable => format!(
            "Shorthand variables cannot have a public modifier. Consider using var instead."
        ),
        ParserErrorType::IdentifierExpected => format!("Identifier expected."),
        ParserErrorType::DeclarationExpected => format!("Declaration expected."),
        ParserErrorType::Expected(t) => format!("Expected {:?}", t),
        ParserErrorType::AsyncType => format!("Types cannot have async markers."),
        ParserErrorType::PublicAccessTypeOnTest => {
            format!("Test blocks do not require public modifiers.")
        }
        ParserErrorType::GenericArgsInNamespace => format!("Unexpected generic arguments."),
        ParserErrorType::UnexpectedToken => format!("Unexpected token."),
        ParserErrorType::StringExpected => format!("Expected a string."),
        ParserErrorType::ExpressionExpected => format!("Expression expected."),
    }
}

/// Stringify a lexer error.
pub fn stringify_lex_error(error: &LexErrorType) -> String {
    match error {
        LexErrorType::InvalidCharacter(c) => {
            format!("Invalid character {c}.")
        }
        LexErrorType::UnterminatedString => format!("Unterminated string."),
        LexErrorType::ExponentforInvalidBase => {
            format!("This number base cannot have an exponent.")
        }
        LexErrorType::NoValAfterExponent => format!("Expected value after exponent."),
        LexErrorType::UnexpectedEndOfInput => format!("Unexpected end of input."),
    }
}
