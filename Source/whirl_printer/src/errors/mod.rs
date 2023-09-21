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
        TypeErrorType::ValueAsType => {
            format!("This refers to a value, but it is being used as a type.")
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
        TypeErrorType::GlobalControl => format!("Control statements and expressions are not allowed in the global scope. Consider moving into a function instead."),
        TypeErrorType::TestInNonGlobalScope => format!("Test declarations are only allowed in the global scope."),
        TypeErrorType::EnumInModelPlace { name } => format!("{name} refers to an enum, but it is being used as a model here."),
        TypeErrorType::TypeInModelPlacd => format!("Type aliases cannot be used in model instantiations."),
        TypeErrorType::InvalidNewExpression => format!("This expression is not constructable."),
        TypeErrorType::ExpectedModelGotAbstract(eval) => format!("Expected a model, got abstract type '{}'", stringify_type_eval(module_ambience, eval)),
        TypeErrorType::UnconstructableModel(name) => format!("'{name}' has no constructor, and therefore cannot be instantiated."),
        TypeErrorType::MismatchedModelArgs { name, expected, assigned } => {
            format!("'{name}' expects {expected} constructor arguments, but got {assigned}.")
        },
        TypeErrorType::UninferrableParameter(name) => format!("Cannot infer the type of parameter '{name}'. Please provide a type label."),
        TypeErrorType::NamelessModule => format!("All modules must have a module declaration."),
        
        
    }
}

/// Stringify a parse error
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
        ParserErrorType::DuplicateConstructor => format!("A model can only have at most one constructor function."),
        
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
