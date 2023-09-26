use whirl_ast::Span;

pub struct ContextError {
    pub _type: ContextErrorType,
    pub span: Span,
}

pub enum ContextErrorType {
    /// Using a property that does not exist.
    UnknownProperty {
        model_name: String,
        property: String,
    },
    /// Using a variable that does not exist.
    UnknownVariableInScope { name: String },
}

pub fn unknown_variable_in_scope(name: String, span: whirl_ast::Span) -> ContextError {
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
