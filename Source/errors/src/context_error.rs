use ast::Span;

#[derive(Debug, PartialEq)]
pub struct ContextError {
    pub _type: ContextErrorType,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ContextErrorType {
    /// Using a property that does not exist.
    UnknownProperty {
        model_name: String,
        property: String,
    },
    /// Using a variable that does not exist.
    UnknownValue {
        name: String,
    },
    /// Redeclaring a block scoped variable.
    AlreadyDeclaredInScope {
        name: String,
    },
    /// Variable is used in its scope before it exists.
    UseBeforeDeclare {
        name: String,
    },
    /// "this" is used outside a trait or model method.
    ThisOutsideMethod,
    /// Declaring two properties with the same name.
    DuplicateModelProperty {
        name: String,
    },
    /// Declaring two generic parameters with the same name.
    DuplicateGenericParameter {
        name: String,
    },
    // Declaraing two function parameters with the same name.
    DuplicateParameterName {
        name: String,
    },
    // Declaring a required parameter after an optional one.
    RequiredAfterOptional,
}

pub fn unknown_value(name: String, span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UnknownValue { name },
        span,
    }
}

pub fn unknown_property(model_name: String, property: String, span: Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UnknownProperty {
            model_name,
            property,
        },
        span,
    }
}
