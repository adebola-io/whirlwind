use crate::{
    BytecodeError, ContextErrorType, ExecutionError, ImportErrorType, LexErrorType,
    ParserErrorType, TypeErrorType, WarningType,
};

impl std::fmt::Display for TypeErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message =  match self {
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
        TypeErrorType::MismatchedAssignment { target: left, right } => {
            if left == right {
               format!("Expected: '{left} (type 1)', Got: '{right} (type 2)'.",)       
            } else {
                format!("Expected: '{left}', Got: '{right}'.",)
            }
         },
        TypeErrorType::InterfaceAsType { name } => {
            format!("{name} refers to a interface, but it is being used as a type here.")
        }
        TypeErrorType::EnumInModelPlace { name } => format!("{name} refers to an enum, but it is being used as a model here."),
        TypeErrorType::TypeInModelPlace => format!("Type aliases cannot be used in model instantiations."),
        TypeErrorType::InvalidNewExpression => format!("This expression is not constructable."),
        TypeErrorType::ExpectedImplementableGotSomethingElse(name) => format!("Expected a model, interface, enum or generic, got '{name}'."),
        TypeErrorType::UnconstructableModel(name) => format!("'{name}' has no constructor, and therefore cannot be instantiated."),
        TypeErrorType::MismatchedModelArgs { name, expected, assigned } => {
            format!("'{name}' expects {expected} constructor arguments, but got {assigned}.")
        },
        TypeErrorType::UninferrableParameter(name) => format!("Cannot infer the type of parameter '{name}'. Please provide a type label."),
        TypeErrorType::ConstructorAssigntoInstance(name) => format!("A constructor cannot be assigned as an instance. Do you mean `new {name}...`?"),
        TypeErrorType::AttributeAccessOnConstructor { model, attribute_name } => format!("Cannot access an attribute on a model blueprint. Did you mean (new {model}(...)).{attribute_name}?"),
        TypeErrorType::ConstructorNonStaticMethodAccess { model_name, method_name } => format!("{method_name} is not a static method on {model_name}, but it is being used in a static context."),
        TypeErrorType::PrivatePropertyLeak {  property_name } => format!("The '{property_name}' property cannot be accessed publicly."),
        TypeErrorType::AccessingOnInterface { interface_ } => format!("{interface_} refers to a interface, thus its methods cannot be directly accessed. Consider implementing them on models instead."),
        TypeErrorType::TypeAsValue { type_ } => format!("{type_} refers to an abstract type, generic parameter or a type alias, but it is being used as a value here."),
        TypeErrorType::InstanceStaticMethodAccess { model_name, method_name } => format!("{method_name} refers a static function, so it cannot be called by instances of {model_name}."),
        TypeErrorType::MismatchedReturnType { expected, found } => if expected == "{void}" {
            format!("The enclosing function has no return type, but '{found}' is being returned here.")
        } else {format!("The enclosing function expects a return type of '{expected}' but found '{found}'.")},
        TypeErrorType::NoSuchProperty { base_type, property } => format!("Property '{property}' does not exist on a value of type '{base_type}'."),
        TypeErrorType::UnimplementedInterface { offender, _interface } => format!("Operation failed because the type '{offender}' does not implement '{_interface}'."),
        TypeErrorType::NotCallable { caller } => format!("{caller} is not a callable type."),
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
        TypeErrorType::HeterogeneousArray => format!("Array contains two or more values with different types."),
        TypeErrorType::InvalidIndexSubject { name } => format!("{name} is not an indexable type."),
        TypeErrorType::ModelNotConstructable { name } =>  format!("The '{name}' model cannot be instantiated because it has no new() constructor."),
        TypeErrorType::NewOnIdentifier { name } =>format!("Invalid new expression. To create an instance of '{name}', pass in an argument list, i.e. `new {name}(/* arguments */)`"),
        TypeErrorType::InfiniteType => format!("This expression generates a recursive type that is too complex to represent."),
        TypeErrorType::NonBooleanLogic { name } => format!("{name} does not evaluate to a boolean expression."),
        TypeErrorType::InvalidAssignmentTarget => format!("Invalid assignment target."),
        TypeErrorType::MutatingMethod {owner, name} => format!("Cannot reassign a model or interface method, or use it as a standalone value. To use a function dynamically, consider changing '{owner}.{name}' to a function expression."),
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
        TypeErrorType::MissingAnnotationsOrValue => format!("Cannot infer types without type labels or initial values."),
        TypeErrorType::NoDefaultImplFor(name) => format!("{name} cannot be declared without a value because it does not implement Default."),
        TypeErrorType::IllegalArrayDestructure { name } => format!("Cannot destructure array elements from value of type '{name}'."),
        TypeErrorType::IllegalModelDestructure { name } => format!("Cannot destructure from value of type {name} because it is not a model instance."),
        TypeErrorType::DestructuringMethod { .. } => format!("Methods cannot be destructured from models."),
        TypeErrorType::NonPureGlobal => format!("Expression with possible side effects are not allowed in global scope."),
        TypeErrorType::ReturnFromConstructor => format!("Model constructors do not expect a return value."),
        TypeErrorType::UsingAttributeBeforeAssign => format!("Attribute is being used before it is assigned."),
        TypeErrorType::UnassignedAttribute { name } => format!("Attribute must be definitely assigned in the constructor, because the type {name} does not implement Default."),
        TypeErrorType::UninferrableVariable {name} => format!("Cannot fully infer the type of {name}. Consider adding type annotations."),
        TypeErrorType::InvalidSize { error } => error.clone(),
        TypeErrorType::ThisInStaticMethod => format!("The 'this' identifier cannot be used in a static method."),
        TypeErrorType::CompositeError { main_error, sub_errors } => {
            let mut string = format!("- {}", main_error.to_string());
            string += " \n";
            for error in sub_errors {
                string.push_str("- ");
                string += &error.to_string();
                string += " \n"
            }
            string
        },
        TypeErrorType::NotAModuleType { object_type } => format!("{object_type} is not a module."),
        TypeErrorType::NonPublicType { base_type, property } => format!("'{property}' exists in {base_type}, but it is not denoted as public."),
        TypeErrorType::IndexingWithIllegalValue { indexer } => format!("Value of type '{indexer}' cannot be used as an index into an array."),
        TypeErrorType::ImplicitLoopReturn { rettype } => format!("For loop blocks do not return a value, but {rettype} is implicitly returned here."),
        TypeErrorType::Illegalterator { illegal_type } => format!("Iteration invalid because {illegal_type} does not implement Iterable or AsIterator."),
        TypeErrorType::UsingThisBeforeConstructor => format!("Instance cannot be used before it is fully constructed."),
        TypeErrorType::Incomparable { left, right } => format!("Cannot compare {left} to {right}."),
        TypeErrorType::MethodInConstructor => format!("Methods on a model instance cannot be called from inside its constructor."),
        TypeErrorType::NotOrderable { name, operator } => format!("Illegal operator {operator:?}. Values of type {name} do not implement Orderable."),
        TypeErrorType::NotSequenced { name} => format!("Range cannot be created because values of type {name} do not implement Sequenced."),
        TypeErrorType::NumericExclusiveOperation { typ } => format!("{typ} is not a numeric type."),
        TypeErrorType::ExpectedInterface { got } => format!("Expected an interface here, got type '{got}'."),
        TypeErrorType::InvalidDefaultType { name, generic } => format!("{name} cannot be used as a default value for the generic type {generic}."),
        TypeErrorType::DuplicateImplementationOf { name } => format!("Duplicate implementation of interface {name}."),
        TypeErrorType::MissingImplementation { inteface, method } => format!("Missing implementation for virtual method '{inteface}.{method}'."),
        TypeErrorType::ConflictingImplementation { former_interface, next_interface, method } => format!("Conflicting implementations: {former_interface} and {next_interface} provide different methods for {method}."),
        TypeErrorType::MismatchedGenericParam { method_name, expected, got } => format!("Mismatched implementation: {method_name} should have {expected} generic param{}, not {got}.", if *expected == 1 {""} else {"s"}),
        TypeErrorType::MismatchedMethodAccess { method_name, expected, .. } => format!("Mismatched implementation: {method_name}() is {}expected to be public.", if *expected {""} else {"not "}),
        TypeErrorType::MismatchedMethodStatic { method_name, expected, .. } => format!("Mismatched implementation: {method_name}() is {}expected to be static.", if *expected {""} else {"not "}),
        TypeErrorType::MismatchedMethodSignature { method_name, left, right } => format!("Virtual method {method_name}() has signature '{left}', but is implemented as '{right}'."),
        TypeErrorType::IllegalBoundConstraintType => format!("Type constraints are not allowed in this context."),
        TypeErrorType::UnsatisfiableConstraint => format!("The type constraint cannot be true."),
        TypeErrorType::MismatchedMethods { base_name, method_name, first_signature, second_signature } => format!("Constraint produces different signatures for {base_name}.{method_name}, '{first_signature}' and '{second_signature}'."),
        TypeErrorType::MismatchedImplementations { name, first, second } => format!("Constraint produces conflicting implementations '{first}' and '{second}' for {name}"),
        };

        write!(f, "{message}")
    }
}

/// Stringify a parse error
impl std::fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
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
            ParserErrorType::TypeInInterfacePosition(t) => format!(
                "Expected a interface, got a {}",
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
            ParserErrorType::NumericValueInArray => format!("Numeric constraints are not supported in array types."),
            ParserErrorType::TypeConditionExpected => format!("Ternary condition expected."), 
        };
        return write!(f, "{message}");
    }
}

impl std::fmt::Display for LexErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            LexErrorType::InvalidCharacter(c) => {
                format!("Invalid character {c}.")
            }
            LexErrorType::UnterminatedString => format!("Unterminated string."),
            LexErrorType::ExponentforInvalidBase => {
                format!("This number base cannot have an exponent.")
            }
            LexErrorType::NoValAfterExponent => format!("Expected value after exponent."),
            LexErrorType::UnexpectedEndOfInput => format!("Unexpected end of input."),
        };
        return write!(f, "{message}");
    }
}

impl std::fmt::Display for ImportErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
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
        };
        return write!(f, "{message}");
    }
}

impl std::fmt::Display for ContextErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            ContextErrorType::UnknownValue { name } => {
                format!("Use of undeclared variable '{name}'.")
            }
            ContextErrorType::UnknownProperty {
                model_name,
                property,
            } => format!("No property '{property}' exists on {model_name}."),
            ContextErrorType::AlreadyDeclaredInScope { name } => {
                format!("Cannot redeclare block scoped value '{name}'.")
            }
            ContextErrorType::UseBeforeDeclare { name } => {
                format!("Use of block scoped value '{name}' before its declaration.")
            }
            ContextErrorType::ThisOutsideMethod => {
                format!("The `this` value can only be used in model or interface methods.")
            }
            ContextErrorType::DuplicateModelProperty { name } => {
                format!("Duplicate model property '{name}'.")
            }
            ContextErrorType::DuplicateGenericParameter { name } => {
                format!("Duplicate generic parameter '{name}'.")
            }
            ContextErrorType::DuplicateParameterName { name } => {
                format!("Duplicate parameter name '{name}'.")
            }
            ContextErrorType::RequiredAfterOptional => {
                format!("A required parameter cannot follow an optional one.")
            }
            ContextErrorType::DuplicateEnumVariant { name } => {
                format!("Duplicate enum variant '{name}'.")
            }
            ContextErrorType::DuplicateLoopVariable { name } => {
                format!("Duplicate loop variable '{name}'")
            }
            ContextErrorType::NamelessModule => {
                format!("All .wrl files must have a module name declaration.")
            }
            ContextErrorType::MismatchInName { module_name, file_name } => format!("Module name and file names must be equal. This module is named '{module_name}', but the file is named '{file_name}'."),
        };
        write!(f, "{message}")
    }
}

impl std::fmt::Display for WarningType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            WarningType::UnusedImportSymbol(name) => format!("Unused import '{name}'."),
            WarningType::UnusedModelSymbol(name) => {
                format!("'{name}' is never constructed or accessed statically.")
            }
            WarningType::RedundantConstraint => format!("Redundant type constraint."),
        };
        write!(f, "{message}")
    }
}

impl std::fmt::Display for BytecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            BytecodeError::MainIsAsync => "The entry main() function cannot be async.",
            BytecodeError::MainReturns => "The entry main() function cannot have a return type.",
            BytecodeError::MainNotFound => "main() function not found in entry file.",
            BytecodeError::MainHasParameters => "The entry main() function cannot have parameters.",
        };
        write!(f, "{message}")
    }
}

impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            ExecutionError::MainCrashed => "Main sequence crashed.",
            ExecutionError::MainFunctionNotDefined => "Main function not defined.",
            ExecutionError::StackOverflow => "Main sequence has overflown its stack.",
            ExecutionError::IllegalMemoryAccess => "Illegal memory access.",
        };
        write!(f, "{message}")
    }
}
