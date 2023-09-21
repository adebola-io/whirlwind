use whirl_ast::{BinOperator, Span, TypeEval};

/// A type checking error.
#[derive(Debug, PartialEq)]
pub struct TypeError {
    pub _type: TypeErrorType,
    // Affected area.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorType {
    /// Performing a binary operation on incompatible types.
    InvalidBinary {
        left: TypeEval,
        operator: BinOperator,
        right: TypeEval,
    },
    /// Annotating with `: invalid`.
    AssignedInvalid,
    /// Using an undeclared type.
    UnknownType {
        name: String,
    },
    /// Using a variable name in a type annotation.
    ValueAsType,
    /// Giving generic arguments to a type that is not generic.
    UnexpectedGenericArgs {
        value: String,
    },
    /// Incompatible number of generic arguments to parameters.
    MismatchedGenericArgs {
        value: String,
        expected: usize,
        assigned: usize,
    },
    /// Assigning two unassignable types.
    MismatchedAssignment {
        left: TypeEval,
        right: TypeEval,
    },
    /// Using a trait in a type expression.
    TraitAsType {
        name: String,
    },
    /// Using a variable that does not exist.
    UnknownVariableInScope {
        name: String,
    },
    /// Global control flow statements.
    GlobalControl,
    /// Writing a test in a local scope.
    TestInNonGlobalScope,
    EnumInModelPlace {
        name: String,
    },
    TypeInModelPlacd,
    InvalidNewExpression,
    ExpectedModelGotAbstract(TypeEval),
    UnconstructableModel(String),
    MismatchedModelArgs {
        name: String,
        expected: usize,
        assigned: usize,
    },
    UninferrableParameter(String),
    NamelessModule,
}

pub fn assigned_invalid(span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AssignedInvalid,
        span,
    }
}

pub fn invalid_binary(
    left: TypeEval,
    operator: BinOperator,
    right: TypeEval,
    span: Span,
) -> TypeError {
    TypeError {
        _type: TypeErrorType::InvalidBinary {
            left,
            operator,
            right,
        },
        span,
    }
}

pub fn unknown_type(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnknownType {
            name: name.to_owned(),
        },
        span,
    }
}

pub fn value_as_type(span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ValueAsType,
        span,
    }
}

pub fn unexpected_generic_args(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnexpectedGenericArgs {
            value: name.to_owned(),
        },
        span,
    }
}

pub fn mismatched_generics(name: &str, expected: usize, assigned: usize, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedGenericArgs {
            value: name.to_owned(),
            expected,
            assigned,
        },
        span,
    }
}

pub fn mismatched_assignment(left: TypeEval, right: TypeEval, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedAssignment { left, right },
        span,
    }
}

pub fn trait_as_type(name: &str, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TraitAsType {
            name: name.to_owned(),
        },
        span,
    }
}

pub fn enum_in_model_place(name: &str, span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::EnumInModelPlace {
            name: name.to_owned(),
        },
        span,
    }
}

pub fn type_in_model_place(span: whirl_ast::Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::TypeInModelPlacd,
        span,
    }
}

pub fn expected_model_got_abstract(typeval: TypeEval, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ExpectedModelGotAbstract(typeval),
        span,
    }
}
