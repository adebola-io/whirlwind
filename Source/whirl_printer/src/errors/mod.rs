
use whirl_errors::{LexErrorType, ParserErrorType, TypeErrorType};

/// Stringify a type error.
pub fn stringify_type_error(error: &TypeErrorType) -> String {
    match error {
        TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        } => format!(
            "Operator '{:?}' is not defined for {left} and {right}.",
            operator,
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
        TypeErrorType::UnexpectedGenericArgs { name } => format!("'{name}' is not generic."),
        TypeErrorType::MismatchedGenericArgs {
            name,
            expected,
            assigned,
        } => format!("'{name}' expects {expected} generic arguments, but got {assigned} instead."),
        TypeErrorType::MismatchedAssignment { left, right } => format!(
            "Cannot assign '{left}' to '{right}'.",
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
        TypeErrorType::TypeInModelPlace => format!("Type aliases cannot be used in model instantiations."),
        TypeErrorType::InvalidNewExpression => format!("This expression is not constructable."),
        TypeErrorType::ExpectedModelGotAbstract(name) => format!("Expected a model, got abstract type '{name}'"),
        TypeErrorType::UnconstructableModel(name) => format!("'{name}' has no constructor, and therefore cannot be instantiated."),
        TypeErrorType::MismatchedModelArgs { name, expected, assigned } => {
            format!("'{name}' expects {expected} constructor arguments, but got {assigned}.")
        },
        TypeErrorType::UninferrableParameter(name) => format!("Cannot infer the type of parameter '{name}'. Please provide a type label."),
        TypeErrorType::NamelessModule => format!("All modules must have a module declaration."),
        TypeErrorType::ConstructorAssigntoInstance(name) => format!("A constructor cannot be assigned as an instance. Do you mean `new {name}...`?"),
        TypeErrorType::NoPropOnEval(eval, name) => format!("No property '{name}' on '{eval}'"),
        TypeErrorType::AttributeAccessOnConstructor { model, attribute_name } => format!("Cannot access an attribute on a model blueprint. Did you mean (new {model}(...)).{attribute_name}?"),
        TypeErrorType::ConstructorNonStaticMethodAccess { model_name, method_name } => format!("{method_name} is not a static method on {model_name}, but it is being used in a static context."),
        TypeErrorType::PrivatePropertyLeak { model_name, property_name } => format!("{property_name} exists on {model_name}, but it cannot be accessed publicly."),
        TypeErrorType::UnknownProperty { model_name, property } => format!("No property '{property}' exists on {model_name}."),
        TypeErrorType::AccessingOnTrait { trait_ } => format!("{trait_} refers to a trait, thus its methods cannot be directly accessed. Consider implementing them on models instead."),
        TypeErrorType::TypeAsValue { type_ } => format!("{type_} refers to an abstract type or a type alias, but it is being used as a value here."),
        TypeErrorType::InstanceStaticMethodAccess { model_name, method_name } => format!("{method_name} refers a static function, so it cannot be called by instances of {model_name}."),
        
        
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
        ParserErrorType::EmptyPathList => format!("Empty list of items to use. Remove the '{{}}' to import the whole module."),
        
        
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
