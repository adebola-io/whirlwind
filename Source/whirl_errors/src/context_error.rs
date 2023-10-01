use whirl_ast::Span;

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
    UnknownVariableInScope {
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
    ThisOutsideMethod,
}

pub fn unknown_value(name: String, span: whirl_ast::Span) -> ContextError {
    ContextError {
        _type: ContextErrorType::UnknownVariableInScope { name },
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
