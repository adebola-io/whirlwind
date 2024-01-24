use ast::{BinOperator, Span};

/// A type checking error.
#[derive(Debug, PartialEq)]
pub struct TypeError {
    pub _type: TypeErrorType,
    // Affected area.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorType {
    CompositeError {
        main_error: Box<TypeErrorType>,
        sub_errors: Vec<TypeErrorType>,
    },
    /// Performing a binary operation on incompatible types.
    InvalidBinary {
        left: String,
        operator: BinOperator,
        right: String,
    },
    /// Using a variable name in a type annotation.
    ValueAsType {
        name: String,
    },
    /// Giving generic arguments to a type that is not generic.
    UnexpectedGenericArgs {
        name: String,
    },
    /// Incompatible number of generic arguments to parameters.
    MismatchedGenericArgs {
        name: String,
        expected: usize,
        assigned: usize,
    },
    /// Assigning two unassignable types.
    MismatchedAssignment {
        target: String,
        right: String,
    },
    /// Using a interface in a type expression.
    InterfaceAsType {
        name: String,
    },

    EnumInModelPlace {
        name: String,
    },
    TypeInModelPlace,
    InvalidNewExpression,
    ExpectedImplementableGotSomethingElse(String),
    UnconstructableModel(String),
    MismatchedModelArgs {
        name: String,
        expected: usize,
        assigned: usize,
    },
    UninferrableParameter(String),
    ConstructorAssigntoInstance(String),
    AttributeAccessOnConstructor {
        model: String,
        attribute_name: String,
    },
    ConstructorNonStaticMethodAccess {
        model_name: String,
        method_name: String,
    },
    PrivatePropertyLeak {
        property_name: String,
    },
    AccessingOnInterface {
        interface_: String,
    },
    TypeAsValue {
        type_: String,
    },
    InstanceStaticMethodAccess {
        model_name: String,
        method_name: String,
    },
    MismatchedReturnType {
        expected: String,
        found: String,
    },
    NoSuchProperty {
        base_type: String,
        property: String,
    },
    UnimplementedInterface {
        offender: String,
        _interface: String,
    },
    /// Calling a non-callable type.
    NotCallable {
        caller: String,
    },
    /// Calling a function with an incorrect number of arguments.
    MismatchedFunctionArgs {
        expected: usize,
        found: usize,
        least_required: Option<usize>,
    },
    /// Assigning a function with an incorrect number of parameters.
    MismatchedFunctionParams {
        expected: usize,
        found: usize,
        least_required: Option<usize>,
    },
    /// One of the instrinsic symbols is not available.
    MissingIntrinsic {
        name: String,
    },
    /// One of the functions tabled for unification is not async, while the other is.
    AsyncMismatch {
        async_func: String,
        non_async_func: String,
    },
    /// Array with multiple types.
    HeterogeneousArray,
    /// Index subject cannot be used in an index expression.
    InvalidIndexSubject {
        name: String,
    },
    /// Calling new on a model without a new() function.
    ModelNotConstructable {
        name: String,
    },
    /// Calling new on a model name without passing parameters.
    NewOnIdentifier {
        name: String,
    },
    /// Type Alias references itself recursively.
    InfiniteType,
    NonBooleanLogic {
        name: String,
    },
    /// todo: this should be a syntax error.
    InvalidAssignmentTarget,
    MutatingMethod {
        owner: String,
        name: String,
    },
    AssigningToReference,
    SeparateIfTypes {
        first: String,
        second: String,
    },
    VoidAssignment,
    PartialTypeAssigmentIf,
    NeverAsDeclared,
    MispelledName {
        name: String,
    },
    /// Using a private symbol from another module.
    PrivateSymbolLeak {
        modulename: String,
        property: String,
    },
    /// Accessing a nonexistent symbol from a module.
    NoSuchSymbol {
        modulename: String,
        property: String,
    },
    /// Right type is not a component of left opaque type.
    InvalidOpaqueTypeAssignment {
        left: String,
        right: String,
    },
    /// Left and right types have different components.
    MissingOpaqueComponent {
        left: String,
        right: String,
    },
    /// Using the * operator on a value that cannot be dereferenced.
    InvalidDereference {
        name: String,
    },
    /// Using the ! operator on a value that does not implement Guaranteed.
    IllegalGuarantee {
        name: String,
    },
    /// Using the ? operator on a value that does not implement Guaranteed.
    IllegalTry {
        name: String,
    },
    /// Errors pertaining to parsing numbers.
    NumericConversionError {
        error: String,
    },
    /// Errors pertaining to implicit casting.
    NumericCastingError {
        left: String,
        right: String,
    },
    /// Errors that arise from using variables or parameters without types and values.
    MissingAnnotationsOrValue,
    /// Errors that arise from declaring a variable without a value, and with a type that does not implement Default.
    NoDefaultImplFor(String),
    // Destructuring an item that is not an array.
    IllegalArrayDestructure {
        name: String,
    },
    // Destructuring an item that is not a model instance.
    IllegalModelDestructure {
        name: String,
    },
    DestructuringMethod {
        base_type: String,
        method_name: String,
    },
    NonPureGlobal,
    ReturnFromConstructor,
    UsingAttributeBeforeAssign,
    UnassignedAttribute {
        name: String,
    },
    // InfiniteType,
    UninferrableVariable {
        name: String,
    },
    InvalidSize {
        error: String,
    },
    ThisInStaticMethod,
    /// Object type in Member type is not a module.
    NotAModuleType {
        object_type: String,
    },
    /// Type exists in module, but it is not public.
    NonPublicType {
        base_type: String,
        property: String,
    },
    // Using an invalid type as an index into an array.
    IndexingWithIllegalValue {
        indexer: String,
    },
    ImplicitLoopReturn {
        rettype: String,
    },
    Illegalterator {
        illegal_type: String,
    },
    UsingThisBeforeConstructor,
    Incomparable {
        left: String,
        right: String,
    },
    MethodInConstructor,
    NotOrderable {
        name: String,
        operator: BinOperator,
    },
    NotSequenced {
        name: String,
    },
    NumericExclusiveOperation {
        typ: String,
    },
    ExpectedInterface {
        got: String,
    },
    InvalidDefaultType {
        name: String,
        generic: String,
    },
    DuplicateImplementationOf {
        name: String,
    },
    MissingImplementation {
        inteface: String,
        method: String,
    },
    ConflictingImplementation {
        former_interface: String,
        next_interface: String,
        method: String,
    },
    MismatchedGenericParam {
        method_name: String,
        expected: usize,
        got: usize,
    },
    MismatchedMethodAccess {
        method_name: String,
        got: bool,
        expected: bool,
    },
    MismatchedMethodStatic {
        method_name: String,
        got: bool,
        expected: bool,
    },
    MismatchedMethodSignature {
        method_name: String,
        left: String,
        right: String,
    },
    IllegalBoundConstraintType,
}

pub fn invalid_binary(left: String, operator: BinOperator, right: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        },
        span,
    }
}

pub fn value_as_type(name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ValueAsType { name },
        span,
    }
}

pub fn unexpected_generic_args(name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnexpectedGenericArgs { name },
        span,
    }
}

pub fn mismatched_generics(
    name: String,
    expected: usize,
    assigned: usize,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedGenericArgs {
            name,
            expected,
            assigned,
        },
        span,
    }
}

pub fn mismatched_assignment(left: String, right: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedAssignment {
            target: left,
            right,
        },
        span,
    }
}

pub fn interface_as_type(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InterfaceAsType { name },
        span,
    }
}

pub fn enum_in_model_place(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::EnumInModelPlace { name },
        span,
    }
}

pub fn type_in_model_place(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TypeInModelPlace,
        span,
    }
}

pub fn expected_implementable(name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ExpectedImplementableGotSomethingElse(name),
        span,
    }
}

pub fn invalid_new_expression(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidNewExpression,
        span,
    }
}

pub fn unconstructable_model(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnconstructableModel(name),
        span,
    }
}

pub fn mismatched_model_args(
    name: String,
    expected: usize,
    assigned: usize,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedModelArgs {
            name,
            expected,
            assigned,
        },
        span,
    }
}

pub fn uninferrable_parameter(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UninferrableParameter(name),
        span,
    }
}

pub fn using_constructor_as_value_in_assign(left: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ConstructorAssigntoInstance(left),
        span,
    }
}

pub fn attribute_access_on_contructor(
    model: String,
    attribute_name: String,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::AttributeAccessOnConstructor {
            model,
            attribute_name,
        },
        span,
    }
}

pub fn contructor_non_static_method_access(
    model_name: String,
    method_name: String,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::ConstructorNonStaticMethodAccess {
            model_name,
            method_name,
        },
        span,
    }
}

pub fn private_property_leak(property_name: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::PrivatePropertyLeak { property_name },
        span,
    }
}

pub fn accessing_on_interface(interface_: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AccessingOnInterface { interface_ },
        span,
    }
}

pub fn type_as_value(type_: String, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TypeAsValue { type_ },
        span,
    }
}

pub fn instance_static_method_access(
    model_name: String,
    method_name: String,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::InstanceStaticMethodAccess {
            model_name,
            method_name,
        },
        span,
    }
}

pub fn not_callable(caller: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NotCallable { caller },
        span,
    }
}

pub fn mismatched_function_args(
    span: ast::Span,
    expected: usize,
    found: usize,
    least_required: Option<usize>,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedFunctionArgs {
            expected,
            found,
            least_required,
        },
        span,
    }
}

pub fn missing_intrinsic(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MissingIntrinsic { name },
        span,
    }
}

pub fn invalid_index_subject(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidIndexSubject { name },
        span,
    }
}

pub fn model_not_constructable(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ModelNotConstructable { name },
        span,
    }
}

pub fn calling_new_on_identifier(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NewOnIdentifier { name },
        span,
    }
}

pub fn infinite_type(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InfiniteType,
        span,
    }
}

pub fn non_boolean_logic(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NonBooleanLogic { name },
        span,
    }
}

pub fn invalid_assignment_target(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidAssignmentTarget,
        span,
    }
}

pub fn mutating_method(owner: String, name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MutatingMethod { owner, name },
        span,
    }
}

pub fn assigning_to_reference(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AssigningToReference,
        span,
    }
}

pub fn separate_if_types(span: ast::Span, first: String, second: String) -> TypeError {
    TypeError {
        _type: TypeErrorType::SeparateIfTypes { first, second },
        span,
    }
}

pub fn void_assignment(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::VoidAssignment,
        span,
    }
}

pub fn partial_type_assignment(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::PartialTypeAssigmentIf,
        span,
    }
}

pub fn never_as_declared(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NeverAsDeclared,
        span,
    }
}

pub fn mispelled_name(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MispelledName { name },
        span,
    }
}

pub fn illegal_guarantee(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::IllegalGuarantee { name },
        span,
    }
}

pub fn illegal_try(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::IllegalTry { name },
        span,
    }
}

pub fn missing_annotations(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MissingAnnotationsOrValue,
        span,
    }
}

pub fn no_default(declared: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NoDefaultImplFor(declared),
        span,
    }
}

pub fn illegal_array_destructure(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::IllegalArrayDestructure { name },
        span,
    }
}

pub fn illegal_model_destructure(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::IllegalModelDestructure { name },
        span,
    }
}

pub fn unknown_property(base_type: String, property: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NoSuchProperty {
            base_type,
            property,
        },
        span,
    }
}

pub fn destructuring_method(base_type: String, method_name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::DestructuringMethod {
            base_type,
            method_name,
        },
        span,
    }
}

pub fn non_pure_global(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NonPureGlobal,
        span,
    }
}

pub fn return_from_constructor(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ReturnFromConstructor,
        span,
    }
}

pub fn using_attribute_before_assign(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UsingAttributeBeforeAssign,
        span,
    }
}

pub fn unassigned_attribute(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnassignedAttribute { name },
        span,
    }
}

pub fn uninferrable_variable(name: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UninferrableVariable { name },
        span,
    }
}

pub fn invalid_size(error: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidSize { error },
        span,
    }
}

pub fn this_in_static_method(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ThisInStaticMethod,
        span,
    }
}

pub fn composite_type_error(
    main_error: TypeErrorType,
    sub_errors: Vec<TypeErrorType>,
    span: ast::Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::CompositeError {
            main_error: Box::new(main_error),
            sub_errors,
        },
        span,
    }
}

pub fn not_a_module_type(object_type: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NotAModuleType { object_type },
        span,
    }
}

pub fn non_public_type(base_type: String, property: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::NonPublicType {
            base_type,
            property,
        },
        span,
    }
}

pub fn indexing_with_illegal_value(indexer: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::IndexingWithIllegalValue { indexer },
        span,
    }
}

pub fn implicit_loop_return(rettype: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ImplicitLoopReturn { rettype },
        span,
    }
}

pub fn illegal_iterator(illegal_type: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::Illegalterator { illegal_type },
        span,
    }
}

pub fn using_this_before_construction(span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UsingThisBeforeConstructor,
        span,
    }
}

pub fn incomparable(left: String, right: String, span: ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::Incomparable { left, right },
        span,
    }
}
