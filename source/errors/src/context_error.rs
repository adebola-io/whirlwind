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
    /// "this" is used outside a interface or model method.
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
    // Declaraing two enum variants with the same name.
    DuplicateEnumVariant {
        name: String,
    },
    /// Declaraing two loop variables with the same name.
    DuplicateLoopVariable {
        name: String,
    },
}

pub fn unknown_value(name: String, span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UnknownValue { name },
        span,
    }
}

pub fn already_declared_in_scope(name: String, span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::AlreadyDeclaredInScope { name },
        span,
    }
}

pub fn use_before_declare(name: String, span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UseBeforeDeclare { name },
        span,
    }
}

pub fn this_outside_method(span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::ThisOutsideMethod,
        span,
    }
}

pub fn duplicate_property(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateModelProperty {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn duplicate_generic_parameter(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateGenericParameter {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn duplicate_parameter_names(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateParameterName {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn required_parameter_after_optional(span: ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::RequiredAfterOptional,
        span,
    }
}

pub fn duplicate_enum_variant(name: ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateEnumVariant {
            name: name.name.to_owned(),
        },
        span: name.span,
    }
}

pub fn duplicate_loop_variable(name: &ast::Identifier) -> ContextError {
    ContextError {
        _type: ContextErrorType::DuplicateLoopVariable {
            name: name.name.clone(),
        },
        span: name.span,
    }
}
