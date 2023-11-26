
use errors::{LexErrorType, ParserErrorType, TypeErrorType, ImportErrorType, ContextErrorType};

use crate::SymbolWriter;

/// Stringify a type error.
pub fn stringify_type_error(error: &TypeErrorType, _writer: SymbolWriter) -> String {
    match error {
        TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        } => format!("Operator '{operator:?}' is not defined for {left} and {right}."),
        TypeErrorType::ValueAsType {name} => {
            format!("{name} refers to a value, but it is being used as a type here.")
        }
        TypeErrorType::UnexpectedGenericArgs { name } => format!("'{name}' is not generic."),
        TypeErrorType::MismatchedGenericArgs {
            name,
            expected,
            assigned,
        } => format!("'{name}' expects {expected} generic argument{}, but got {assigned} instead.", if *expected == 1 {""} else {"s"}),
        TypeErrorType::MismatchedAssignment { left, right } => format!(
            "Expected: '{left}'
Got: '{right}'.",
        ),
        TypeErrorType::TraitAsType { name } => {
            format!("{name} refers to a trait, but it is being used as a type here.")
        }
        TypeErrorType::EnumInModelPlace { name } => format!("{name} refers to an enum, but it is being used as a model here."),
        TypeErrorType::TypeInModelPlace => format!("Type aliases cannot be used in model instantiations."),
        TypeErrorType::InvalidNewExpression => format!("This expression is not constructable."),
        TypeErrorType::ExpectedImplementableGotSomethingElse(name) => format!("Expected a model or a trait, got '{name}'."),
        TypeErrorType::UnconstructableModel(name) => format!("'{name}' has no constructor, and therefore cannot be instantiated."),
        TypeErrorType::MismatchedModelArgs { name, expected, assigned } => {
            format!("'{name}' expects {expected} constructor arguments, but got {assigned}.")
        },
        TypeErrorType::UninferrableParameter(name) => format!("Cannot infer the type of parameter '{name}'. Please provide a type label."),
        TypeErrorType::ConstructorAssigntoInstance(name) => format!("A constructor cannot be assigned as an instance. Do you mean `new {name}...`?"),
        TypeErrorType::AttributeAccessOnConstructor { model, attribute_name } => format!("Cannot access an attribute on a model blueprint. Did you mean (new {model}(...)).{attribute_name}?"),
        TypeErrorType::ConstructorNonStaticMethodAccess { model_name, method_name } => format!("{method_name} is not a static method on {model_name}, but it is being used in a static context."),
        TypeErrorType::PrivatePropertyLeak { model_name, property_name } => format!("{property_name} exists on {model_name}, but it cannot be accessed publicly."),
        TypeErrorType::AccessingOnTrait { trait_ } => format!("{trait_} refers to a trait, thus its methods cannot be directly accessed. Consider implementing them on models instead."),
        TypeErrorType::TypeAsValue { type_ } => format!("{type_} refers to an abstract type, generic parameter or a type alias, but it is being used as a value here."),
        TypeErrorType::InstanceStaticMethodAccess { model_name, method_name } => format!("{method_name} refers a static function, so it cannot be called by instances of {model_name}."),
        TypeErrorType::MismatchedReturnType { expected, found } => if expected == "{void}" {
            format!("The enclosing function has no return type, but '{found}' is being returned here.")
        } else {format!("The enclosing function expects a return type of '{expected}' but found '{found}'.")},
        TypeErrorType::NoSuchProperty { base_type, property } => format!("Property '{property}' does not exist on a value of type '{base_type}'."),
        TypeErrorType::UnimplementedTrait { offender, _trait } => format!("Assignment failed because the type '{offender}' does not implement '{_trait}'."),
        TypeErrorType::NotCallable { caller } => format!("{caller} is not a callable type."),
        TypeErrorType::IllegalModelCall { name } => format!("'{name}' refers to a model, which can be constructed, rather than called. Did you mean to contruct it with `new {name}(...)`?"),
        TypeErrorType::MismatchedFunctionArgs { expected, found, least_required } => match least_required {
            Some(least) => format!("Requires at least {least} argument{}, but {found} {} given.", if *least == 1 {""} else {"s"}, if *found < 2 {"was"} else {"were"}),
            None => format!("Expects {expected} argument{}, but {found} {} given.", if *expected == 1 {""} else {"s"}, if *found < 2 {"was"} else {"were"}),
        },
        TypeErrorType::MismatchedFunctionParams { expected, found, least_required } => match least_required {
            Some(least) => format!("Requires at least {least} parameter{}, but this function has only {found}.", if *least == 1 {""} else {"s"}),
            None => format!("Expects {expected} parameter{}, but this function has {found}.", if *expected == 1 {""} else {"s"},),
        },
        TypeErrorType::MissingIntrinsic { name } => format!("Intrinsic symbol '{name}' could not be resolved. The core library might have been altered or installed incorrectly."),
        TypeErrorType::AsyncMismatch { async_func, non_async_func } => format!("Cannot assign '{async_func}' to '{non_async_func}', because the first function is asynchronous, while the other is not."),
        TypeErrorType::HeterogeneousArray => format!("Array contains two or more values on different types."),
        TypeErrorType::InvalidIndexSubject { name } => format!("{name} is not an indexable type."),
        TypeErrorType::ModelNotConstructable { name } =>  format!("The '{name}' model cannot be instantiated because it has no new() constructor."),
        TypeErrorType::NewOnIdentifier { name } =>format!("Invalid new expression. To create an instance of '{name}', pass in an argument list, i.e. `new {name}(/* arguments */)`"),
        TypeErrorType::InfiniteType => format!("This expression generates a recursive type that is too complex to represent."),
        TypeErrorType::NonBooleanLogic { name } => format!("{name} does not evaluate to a boolean expression."),
        TypeErrorType::InvalidAssignmentTarget => format!("Invalid assignment target."),
        TypeErrorType::MutatingMethod {owner, name} => format!("Cannot reassign a model or trait method, or use it as a standalone value. To use a function dynamically, consider changing '{owner}.{name}' to a function expression."),
        TypeErrorType::AssigningToReference => format!("Cannot assign to a reference value. Consider dereferencing the value with '*' to assign to its source."),
        TypeErrorType::SeparateIfTypes {first, second} => format!("If statement control flow resolves to different types, '{first}' and '{second}'"),
        TypeErrorType::VoidAssignment => format!("Expression evaluates to void type, so it cannot be assigned to a value."),
        TypeErrorType::PartialTypeAssigmentIf => format!("This expression does not fully handle all cases. Consider adding an else clause for exhaustiveness."),
        TypeErrorType::NeverAsDeclared => format!("Types that evaluate to never are not allowed in variable type labels."),
        TypeErrorType::MispelledName { name } => format!("Did you mean '{name}'?"),
        TypeErrorType::PrivateSymbolLeak { modulename, property } => format!("'{property}' exists in module '{modulename}', but it is not denoted as public."),
        TypeErrorType::NoSuchSymbol { modulename, property } => format!("{modulename} has no public member called '{property}'."),
        TypeErrorType::InvalidOpaqueTypeAssignment { left, right } => format!("Assignment failed because '{right}' is not a possible form for opaque type '{left}'."),
        TypeErrorType::MissingOpaqueComponent { left, right } => format!("Assignment failed because '{left}' and '{right}' have different component types."),
        TypeErrorType::InvalidDereference { name } => format!("Value of type {name} cannot be dereferenced."),
        TypeErrorType::IllegalGuarantee { name } => format!("! cannot be used here because type {name} does not implement Guaranteed."),
        TypeErrorType::IllegalTry { name } => format!("? cannot be used here because type {name} does not implement Try."),
        TypeErrorType::NumericConversionError { error } => format!("Numeric conversion error: {error}."),
        TypeErrorType::NumericCastingError { left, right } => format!("Numeric type {left} cannot be assigned a value of type {right}."), 
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
                ast::TypeExpression::Invalid => "invalid type.",
                ast::TypeExpression::Functional(_) => "function type.",
                ast::TypeExpression::This { .. } => "This type.",
                ast::TypeExpression::Union(_) => "union type.",
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
        ParserErrorType::PublicAccessInNonGlobalScope => format!("Public declarations are only allowed in the global scope."),
        ParserErrorType::GlobalControl => format!("Control statements and expressions are not allowed in the global scope. Consider moving into a function instead."),
        ParserErrorType::TestInNonGlobalScope => format!("Test declarations are only allowed in the global scope."),
        ParserErrorType::UseImportInNonGlobalScope => format!("Use imports are only allowed in testing and the global scope."),
        ParserErrorType::EmptyEnumTag => format!("Enum variants cannot have empty tags. Remove the parenthesis if no tags are necessary."),
        ParserErrorType::ContinueOutsideLoop => format!("continue statements can only be used from within a loop."),
        ParserErrorType::BreakOutsideLoop => format!("break statements can only be used from within a loop."),
        
        
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


pub fn stringify_import_error(error: &ImportErrorType) -> String {
    match error {
        ImportErrorType::AmbiguousImport { modulename, offending_files } => format!("There are multiple modules in the current directory that refer to {modulename:?}. Offending files: {:?}", offending_files),
        ImportErrorType::ErrorReadingEntry(err) => format!("Error reading entry file: {err:?}"),
        ImportErrorType::UnknownFileType { path_buf } => format!("Unknown file type for {path_buf:?}"),
        ImportErrorType::ErrorReadingModule { modulename } => format!("An error was encountered while reading module {modulename}"),
        ImportErrorType::VagueAccessError(error) => format!("Vague Access Error: {error:?}"),
        ImportErrorType::NonExistentModule(modulename) => format!("Could not resolve module '{modulename}'."),
        ImportErrorType::DuplicatedModuleNameInSameFolder(modulename) => format!("Module conflict: There is another module named '{modulename}' in this folder."),
        ImportErrorType::SelfReferentialUse(modulename) => format!("Cannot import module '{modulename}' into itself."),
        ImportErrorType::MismatchedModuleCasing {mistake, real} => format!("Could not resolve module '{mistake}'. Did you mean '{real}'"),
        ImportErrorType::UsingPrivateSymbol{
            modulename, symbolname} => format!("{symbolname} exists in module {modulename}, but it is not denoted as public."),
        ImportErrorType::UsingUnreachableModule(name) => format!("{name} exists in this project, but it is unreachable from this module."),
        ImportErrorType::ClashWithInternalSymbol(name) => format!("{name} is already declared in thid module."),
        ImportErrorType::ResolvingFromGlobalFile => format!("Cannot resolve from a global file."),
        ImportErrorType::SymbolNotFound { modulename, symbolname } => format!("module '{modulename}' has no public member called '{symbolname}'. "),
        ImportErrorType::SymbolNotAModule { symbolname } => format!("'{symbolname}' is not a module."),
        ImportErrorType::NamelessModule => format!("All .wrl files must have a module name declaration."),
        ImportErrorType::MismatchInName { module_name, file_name } => format!("Module name and file names must be equal. This module is named '{module_name}', but the file is named '{file_name}'."),
        
    }
}

pub fn stringify_context_error(error: &ContextErrorType) -> String {
    match error {
        ContextErrorType::UnknownValue { name } => format!("Use of undeclared variable '{name}'."),
        ContextErrorType::UnknownProperty { model_name, property } => format!("No property '{property}' exists on {model_name}."),
        ContextErrorType::AlreadyDeclaredInScope { name } => format!("Cannot redeclare block scoped value '{name}'."),
        ContextErrorType::UseBeforeDeclare { name } => format!("Use of block scoped value '{name}' before its declaration."),
        ContextErrorType::ThisOutsideMethod => format!("The `this` value can only be used in model or trait methods."),
        ContextErrorType::DuplicateModelProperty { name } => format!("Duplicate model property '{name}'."),
        ContextErrorType::DuplicateGenericParameter { name } => format!("Duplicate generic parameter '{name}'."),
        ContextErrorType::DuplicateParameterName { name } => format!("Duplicate parameter name '{name}'."),
        ContextErrorType::RequiredAfterOptional => format!("A required parameter cannot follow an optional one."),
        ContextErrorType::DuplicateEnumVariant { name } => format!("Duplicate enum variant '{name}'."),
               
    }
}