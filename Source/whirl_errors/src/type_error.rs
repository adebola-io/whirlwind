use whirl_ast::{BinOperator, Span, TypeEval};

/// A type checking error.
#[derive(Debug, PartialEq)]
pub struct TypeError {
    pub _type: TypeErrorType,
    // Affected areas.
    pub spans: Vec<Span>,
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
    UnknownType { name: String },
    /// Using a variable name in a type annotation.
    ValueAsType { name: String },
    /// Giving generic arguments to a type that is not generic.
    UnexpectedGenericArgs { value: String },
    /// Incompatible number of generic arguments to parameters.
    MismatchedGenericArgs {
        value: String,
        expected: usize,
        assigned: usize,
    },
    /// Assigning two unassignable types.
    MismatchedAssignment { left: TypeEval, right: TypeEval },
    /// Using a trait in a type expression.
    TraitAsType { name: String },
}

pub fn assigned_invalid(span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::AssignedInvalid,
        spans: vec![span],
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
        spans: vec![span],
    }
}

pub fn unknown_type(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnknownType {
            name: name.to_owned(),
        },
        spans: vec![span],
    }
}

pub fn value_as_type(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::ValueAsType {
            name: name.to_owned(),
        },
        spans: vec![span],
    }
}

pub fn unexpected_generic_args(name: &str, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::UnexpectedGenericArgs {
            value: name.to_owned(),
        },
        spans: vec![span],
    }
}

pub fn mismatched_generics(name: &str, expected: usize, assigned: usize, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedGenericArgs {
            value: name.to_owned(),
            expected,
            assigned,
        },
        spans: vec![span],
    }
}

pub fn mismatched_assignment(left: TypeEval, right: TypeEval, span: Span) -> TypeError {
    TypeError {
        _type: TypeErrorType::MismatchedAssignment { left, right },
        spans: vec![span],
    }
}
