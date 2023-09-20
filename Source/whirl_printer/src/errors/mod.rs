use whirl_ast::ModuleAmbience;
use whirl_errors::{LexErrorType, ParserErrorType, TypeErrorType};

use crate::stringify_type_eval;

/// Stringify a type error.
pub fn stringify_type_error(module_ambience: &ModuleAmbience, error: &TypeErrorType) -> String {
    match error {
        TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        } => format!(
            "Operator '{:?}' is not defined for '{}' and '{}'.",
            operator,
            stringify_type_eval(module_ambience, &left),
            stringify_type_eval(module_ambience, &right)
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
            stringify_type_eval(module_ambience, &right),
            stringify_type_eval(module_ambience, &left),
        ),
        TypeErrorType::TraitAsType { name } => {
            format!("{name} refers to a trait, but it is being used as a type here.")
        }
        TypeErrorType::UnknownVariableInScope { name } => {
            format!("Use of undeclared variable {name}")
        }
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
        ParserErrorType::TypeInTraitPosition(t) => format!(
            "Expected a trait, got a {}",
            match t {
                whirl_ast::TypeExpression::Invalid => "invalid type.",
                whirl_ast::TypeExpression::Functional(_) => "function type.",
                whirl_ast::TypeExpression::This { .. } => "This type.",
                whirl_ast::TypeExpression::Union(_) => "union type.",
                _ => "type",
            }
        ),
        ParserErrorType::ExpectedAttribute => {
            format!("Expected an attribute starting with var or function.")
        }
        ParserErrorType::PublicAccessTypeOnConstructor => {
            format!("Constructors do not allow public modifiers.")
        }
        ParserErrorType::DuplicateModuleName => {
            format!("Module name is already declared for this file.")
        }
        ParserErrorType::NonGlobalModuleDeclaration => {
            format!("Module name can only be declared in the global context.")
        }
        ParserErrorType::InvalidReturn => {
            format!("Return statements can only be used within a function or method.")
        }
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
